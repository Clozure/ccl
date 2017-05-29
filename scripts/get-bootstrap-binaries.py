#!/usr/bin/env python

# needs Python 2.7

# This script is for the benefit of buildbot workers.  Here's a
# summary of what it does.
#
# First, it creates a directory named "bootstrap" in the ccl directory
# (assumed to be the parent directory of the one this script lives in).
# This is where it will download bootstrapping heap images and interface
# databases.
#
# Next, "git describe --abbrev=0" is used to determine the  most recent
# tag.  This tag is assumed to correspond to a GitHub release.  The
# script then calls GitHub to get all the information about the release.
#
# Finally, the script looks through the release data to determine the
# URLs of the release assets needed for the current platform.  It then
# downloads those assets as needed.
#
# It goes to some trouble to avoid needlessly re-downloading files by
# using ETag and If-None-Match headers.  When making a request that
# returns a 304 status code, it is not supposed to count against the
# GitHub rate limit, but whatever I am doing here seems to use up the
# rate limit allowance anyway.
#
# See https://developer.github.com/v3/#conditional-requests

import sys, os, errno, platform, getopt, requests, json, subprocess;

# GitHub rate-limits anonymous requests (from a single IP address) to
# 60 requests per hour.  The limit for authenticated users is much
# higher.  The buildbot master will arrange to set these environment
# variables with appropriate credentials.
guser = os.getenv("GITHUB_USER")
gtoken = os.getenv("GITHUB_TOKEN")

def git_tag():
    output = subprocess.check_output(["git", "describe", "--abbrev=0"])
    return output.strip()
                               
def ensure_directory_exists(path):
    try:
        os.makedirs(path)
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise

def fetch(url, dest, auth):
    print "fetching", url
    headers = {"accept": "application/octet-stream"}
    r = requests.get(url, headers=headers, auth=auth) 
    if r.status_code == 200:
        with open(dest, "wb") as f:
            for chunk in r.iter_content(1024 * 64):
                f.write(chunk)
        etag = r.headers["etag"]
        return etag
    else:
        r.raise_for_status()
        return None

# ETags for already-downloaded files are saved in a json dictionary in
# a file named "etags.json" in the bootstrapping directory.
#
# Format:
#    {"dx86cl.image.bz2": "\"621c87a2fdbdcce8d9aeb7249589b28c\"",
#     "dx86cl64.image.bz2": "\"...\""}

def load_etags(dir):
    path = os.path.join(dir, "etags.json")
    etags = {}
    try:
        with open(path, "r") as f:
            etags = json.load(f)
    except IOError:
        print "note: couldn't open saved etags file", path
    except ValueError:
        print "note: removing seemingly corrupt saved etags file"
        os.remove(path)
    finally:
        return etags

def save_etags(data, dir):
    old = load_etags(dir)
    new = old.copy()
    new.update(data)

    path = os.path.join(dir, "etags.json")
    with open(path, "w") as f:
        json.dump(new, f)

def main(argv):
    force = False
    verbose = False
    
    try:
        opts, args = getopt.getopt(argv, "fvc:b:")
    except getopt.GetoptError as err:
        print str(err)
        usage()
        sys.exit(1)
    for opt, val in opts:
        if opt == "-f":
            force = True
        elif opt == "-v":
            verbose = True
        else:
            assert False, "unhandled option"

    system = platform.system()
    machine = platform.machine()
            
    if system == "Darwin":
        binaries = ["dx86cl.image.bz2", "dx86cl64.image.bz2",
                    "darwin-x86-headers.tar.bz2",
                    "darwin-x86-headers64.tar.bz2"]
    elif system == "Linux":
        if machine == "armv7l":
            binaries = ["armcl.image.bz2", "arm-headers.tar.bz2"]
        else:
            binaries = ["lx86cl.image.bz2", "lx86cl64.image.bz2",
                        "x86-headers.tar.bz2", "x86-headers64.tar.bz2"]
    elif system == "FreeBSD":
        binaries = ["fx86cl.image.bz2", "fx86cl64.image.bz2"
                    "freebsd-headers.tar.bz2", "freebsd-headers64.tar.bz2"]
    elif system == "SunOS":
        binaries = ["sx86cl.image.gz", "sx86cl64.image.gz",
                    "solarisx86-headers.tar.bz2", "solarisx64-headers.tar.bz2"]
    elif system == "Windows":
        binaries = ["wx86cl.image.zip", "wx86cl64.image.zip",
                    "win32-headers.tar.bz2", "win64-headers64.tar.bz2"]
    else:
        raise RuntimeError("unknown system {0}".format(system))

    scripts_directory = os.path.dirname(os.path.abspath(__file__))
    ccl_directory = os.path.dirname(scripts_directory)
    bootstrap_dir = os.path.join(ccl_directory, "bootstrap")

    ensure_directory_exists(bootstrap_dir)

    if guser and gtoken:
        auth = (guser, gtoken)
    else:
        auth = None

    # It would be nice to be able to do ETag processing for this
    # endpoint so that we could cache the release info.
    # Unfortunately, release assets (which are all included as part of
    # the overall release info) keep a count of how many times they
    # have been downloaded.  This count seems to increase even when a
    # GET request ends up returning a status code of 304 Not Modified.
    # Thus, this very script ends up changing the release info (and
    # hence the ETag) as a side effect of checking whether the
    # bootstrapping binaries have been updated.

    tag = git_tag()        
    url = "https://api.github.com/repos/Clozure/ccl/releases/tags/" + tag
    r = requests.get(url, auth=auth)
    assert r.status_code == 200, r.text

    release_info = r.json()
    assets = release_info["assets"]

    downloads = []
    for a in assets:
        if a["name"] in binaries:
            downloads.append({"name": a["name"], "url": a["url"]})

            
    local_etags = load_etags(bootstrap_dir)
    etags = {}

    for d in downloads:
        name = d["name"]
        url = d["url"]
        dest = os.path.join(bootstrap_dir, name)
        local_etag = local_etags.get(name)
        if force or not os.path.exists(dest):
            etag = fetch(url, dest, auth)
            if etag:
                etags[name] = etag
        elif os.path.exists(dest) and local_etag:
            # make a conditional request to see if the remote file
            # is still the same as the local version
            headers = {"accept": "application/octet-stream",
                       "if-none-match": local_etag}
            r = requests.get(url, headers=headers, auth=auth)
            if r.status_code == 304:
                print name, "is up-to-date"
            elif r.status_code == 200:
                with open(dest, "wb") as f:
                    for chunk in r.iter_content(1024 * 64):
                        f.write(chunk)
                etag = r.headers["etag"]
                if etag:
                    etags[name] = etag
            else:
                r.raise_for_status()
        else:
            # Some other situation.  Just downlaod the asset.
            etag = fetch(url, dest, auth)
            if etag:
                etags[name] = etag
            
    save_etags(etags, bootstrap_dir)

if __name__ == '__main__':
    main(sys.argv[1:])
