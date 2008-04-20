



<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<!-- ViewVC - http://viewvc.org/
by Greg Stein - mailto:gstein@lyra.org -->
<head>
<title>SourceForge.net Repository - [cclan] View of /asdf/asdf.lisp</title>
<meta name="generator" content="ViewVC 1.0.3" />
<link rel="stylesheet" href="/*docroot*/styles.css" type="text/css" />
</head>
<body>
<table style="padding:0.1em;">
<tr>
<td>
<strong>

<a href="/cclan/">

[cclan]</a>
/

<a href="/cclan/asdf/">

asdf</a>
/

<a href="/cclan/asdf/asdf.lisp?view=log">

asdf.lisp</a>


</strong>

</td>
</tr>
</table>


<div style="float: right; padding: 5px;"><a href="http://sourceforge.net"><img src="/*docroot*/images/sflogo-210pxtrans.png" alt="(logo)" border=0 width=210 height=62></a></div>
<h1>View of /asdf/asdf.lisp</h1>

<p style="margin:0;">

<a href="/cclan/asdf/"><img src="/*docroot*/images/back_small.png" width="16" height="16" alt="Parent Directory" /> Parent Directory</a>

| <a href="/cclan/asdf/asdf.lisp?view=log#rev1.115"><img src="/*docroot*/images/log.png" width="16" height="16" alt="Revision Log" /> Revision Log</a>




</p>

<hr />
<div class="vc_summary">
Revision <strong>1.115</strong> -
(<a href="/*checkout*/cclan/asdf/asdf.lisp?revision=1.115"><strong>download</strong></a>)

(<a href="/cclan/asdf/asdf.lisp?annotate=1.115"><strong>annotate</strong></a>)

<br /><em>Fri Feb 15 12:14:48 2008 UTC</em>
(2 months ago)
by <em>demoss</em>


<br />Branch: <strong>MAIN</strong>


<br />CVS Tags: <strong>HEAD</strong>




<br />Changes since <strong>1.114: +2 -2 lines</strong>





<pre class="vc_log">fix CVS revision magic in *asdf-revision*

 gah.
</pre>

</div>
<div id="vc_markup"><pre><a id="l_1"></a><span class="hl line">    1 </span><span class="hl slc">;;; This is asdf: Another System Definition Facility.  $Revision$</span>
<a id="l_2"></a><span class="hl line">    2 </span><span class="hl slc">;;;</span>
<a id="l_3"></a><span class="hl line">    3 </span><span class="hl slc">;;; Feedback, bug reports, and patches are all welcome: please mail to</span>
<a id="l_4"></a><span class="hl line">    4 </span><span class="hl slc">;;; &lt;cclan-list&#64;lists.sf.net&gt;.  But note first that the canonical</span>
<a id="l_5"></a><span class="hl line">    5 </span><span class="hl slc">;;; source for asdf is presently the cCLan CVS repository at</span>
<a id="l_6"></a><span class="hl line">    6 </span><span class="hl slc">;;; &lt;URL:http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/cclan/asdf/&gt;</span>
<a id="l_7"></a><span class="hl line">    7 </span><span class="hl slc">;;;</span>
<a id="l_8"></a><span class="hl line">    8 </span><span class="hl slc">;;; If you obtained this copy from anywhere else, and you experience</span>
<a id="l_9"></a><span class="hl line">    9 </span><span class="hl slc">;;; trouble using it, or find bugs, you may want to check at the</span>
<a id="l_10"></a><span class="hl line">   10 </span><span class="hl slc">;;; location above for a more recent version (and for documentation</span>
<a id="l_11"></a><span class="hl line">   11 </span><span class="hl slc">;;; and test files, if your copy came without them) before reporting</span>
<a id="l_12"></a><span class="hl line">   12 </span><span class="hl slc">;;; bugs.  There are usually two &quot;supported&quot; revisions - the CVS HEAD</span>
<a id="l_13"></a><span class="hl line">   13 </span><span class="hl slc">;;; is the latest development version, whereas the revision tagged</span>
<a id="l_14"></a><span class="hl line">   14 </span><span class="hl slc">;;; RELEASE may be slightly older but is considered `stable'</span>
<a id="l_15"></a><span class="hl line">   15 </span>
<a id="l_16"></a><span class="hl line">   16 </span><span class="hl slc">;;; Copyright (c) 2001-2007 Daniel Barlow and contributors</span>
<a id="l_17"></a><span class="hl line">   17 </span><span class="hl slc">;;;</span>
<a id="l_18"></a><span class="hl line">   18 </span><span class="hl slc">;;; Permission is hereby granted, free of charge, to any person obtaining</span>
<a id="l_19"></a><span class="hl line">   19 </span><span class="hl slc">;;; a copy of this software and associated documentation files (the</span>
<a id="l_20"></a><span class="hl line">   20 </span><span class="hl slc">;;; &quot;Software&quot;), to deal in the Software without restriction, including</span>
<a id="l_21"></a><span class="hl line">   21 </span><span class="hl slc">;;; without limitation the rights to use, copy, modify, merge, publish,</span>
<a id="l_22"></a><span class="hl line">   22 </span><span class="hl slc">;;; distribute, sublicense, and/or sell copies of the Software, and to</span>
<a id="l_23"></a><span class="hl line">   23 </span><span class="hl slc">;;; permit persons to whom the Software is furnished to do so, subject to</span>
<a id="l_24"></a><span class="hl line">   24 </span><span class="hl slc">;;; the following conditions:</span>
<a id="l_25"></a><span class="hl line">   25 </span><span class="hl slc">;;;</span>
<a id="l_26"></a><span class="hl line">   26 </span><span class="hl slc">;;; The above copyright notice and this permission notice shall be</span>
<a id="l_27"></a><span class="hl line">   27 </span><span class="hl slc">;;; included in all copies or substantial portions of the Software.</span>
<a id="l_28"></a><span class="hl line">   28 </span><span class="hl slc">;;;</span>
<a id="l_29"></a><span class="hl line">   29 </span><span class="hl slc">;;; THE SOFTWARE IS PROVIDED &quot;AS IS&quot;, WITHOUT WARRANTY OF ANY KIND,</span>
<a id="l_30"></a><span class="hl line">   30 </span><span class="hl slc">;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF</span>
<a id="l_31"></a><span class="hl line">   31 </span><span class="hl slc">;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND</span>
<a id="l_32"></a><span class="hl line">   32 </span><span class="hl slc">;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE</span>
<a id="l_33"></a><span class="hl line">   33 </span><span class="hl slc">;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION</span>
<a id="l_34"></a><span class="hl line">   34 </span><span class="hl slc">;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION</span>
<a id="l_35"></a><span class="hl line">   35 </span><span class="hl slc">;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.</span>
<a id="l_36"></a><span class="hl line">   36 </span>
<a id="l_37"></a><span class="hl line">   37 </span><span class="hl slc">;;; the problem with writing a defsystem replacement is bootstrapping:</span>
<a id="l_38"></a><span class="hl line">   38 </span><span class="hl slc">;;; we can't use defsystem to compile it.  Hence, all in one file</span>
<a id="l_39"></a><span class="hl line">   39 </span>
<a id="l_40"></a><span class="hl line">   40 </span><span class="hl sym">(</span>defpackage #<span class="hl sym">:</span>asdf
<a id="l_41"></a><span class="hl line">   41 </span>  <span class="hl sym">(:</span>export #<span class="hl sym">:</span>defsystem #<span class="hl sym">:</span>oos #<span class="hl sym">:</span>operate #<span class="hl sym">:</span>find-system #<span class="hl sym">:</span>run-shell-<span class="hl kwa">command</span>
<a id="l_42"></a><span class="hl line">   42 </span>           #<span class="hl sym">:</span>system-definition-pathname #<span class="hl sym">:</span>find-component <span class="hl slc">; miscellaneous</span>
<a id="l_43"></a><span class="hl line">   43 </span>           #<span class="hl sym">:</span>hyperdocumentation #<span class="hl sym">:</span>hyperdoc
<a id="l_44"></a><span class="hl line">   44 </span>
<a id="l_45"></a><span class="hl line">   45 </span>           #<span class="hl sym">:</span>compile-op #<span class="hl sym">:</span><span class="hl kwa">load</span>-op #<span class="hl sym">:</span><span class="hl kwa">load</span>-source-op #<span class="hl sym">:</span>test-system-version
<a id="l_46"></a><span class="hl line">   46 </span>           #<span class="hl sym">:</span>test-op
<a id="l_47"></a><span class="hl line">   47 </span>           #<span class="hl sym">:</span>operation                  <span class="hl slc">; operations</span>
<a id="l_48"></a><span class="hl line">   48 </span>           #<span class="hl sym">:</span>feature                    <span class="hl slc">; sort-of operation</span>
<a id="l_49"></a><span class="hl line">   49 </span>           #<span class="hl sym">:</span>version                    <span class="hl slc">; metaphorically sort-of an operation</span>
<a id="l_50"></a><span class="hl line">   50 </span>
<a id="l_51"></a><span class="hl line">   51 </span>           #<span class="hl sym">:</span>input-files #<span class="hl sym">:</span>output-files #<span class="hl sym">:</span>perform       <span class="hl slc">; operation methods</span>
<a id="l_52"></a><span class="hl line">   52 </span>           #<span class="hl sym">:</span>operation-done-p #<span class="hl sym">:</span>explain
<a id="l_53"></a><span class="hl line">   53 </span>
<a id="l_54"></a><span class="hl line">   54 </span>           #<span class="hl sym">:</span>component #<span class="hl sym">:</span>source-file
<a id="l_55"></a><span class="hl line">   55 </span>           #<span class="hl sym">:</span>c-source-file #<span class="hl sym">:</span>cl-source-file #<span class="hl sym">:</span>java-source-file
<a id="l_56"></a><span class="hl line">   56 </span>           #<span class="hl sym">:</span>static-file
<a id="l_57"></a><span class="hl line">   57 </span>           #<span class="hl sym">:</span>doc-file
<a id="l_58"></a><span class="hl line">   58 </span>           #<span class="hl sym">:</span>html-file
<a id="l_59"></a><span class="hl line">   59 </span>           #<span class="hl sym">:</span>text-file
<a id="l_60"></a><span class="hl line">   60 </span>           #<span class="hl sym">:</span>source-file-<span class="hl kwa">type</span>
<a id="l_61"></a><span class="hl line">   61 </span>           #<span class="hl sym">:</span>module                     <span class="hl slc">; components</span>
<a id="l_62"></a><span class="hl line">   62 </span>           #<span class="hl sym">:</span>system
<a id="l_63"></a><span class="hl line">   63 </span>           #<span class="hl sym">:</span>unix-dso
<a id="l_64"></a><span class="hl line">   64 </span>
<a id="l_65"></a><span class="hl line">   65 </span>           #<span class="hl sym">:</span>module-components          <span class="hl slc">; component accessors</span>
<a id="l_66"></a><span class="hl line">   66 </span>           #<span class="hl sym">:</span>component-pathname
<a id="l_67"></a><span class="hl line">   67 </span>           #<span class="hl sym">:</span>component-relative-pathname
<a id="l_68"></a><span class="hl line">   68 </span>           #<span class="hl sym">:</span>component-name
<a id="l_69"></a><span class="hl line">   69 </span>           #<span class="hl sym">:</span>component-version
<a id="l_70"></a><span class="hl line">   70 </span>           #<span class="hl sym">:</span>component-parent
<a id="l_71"></a><span class="hl line">   71 </span>           #<span class="hl sym">:</span>component-property
<a id="l_72"></a><span class="hl line">   72 </span>           #<span class="hl sym">:</span>component-system
<a id="l_73"></a><span class="hl line">   73 </span>
<a id="l_74"></a><span class="hl line">   74 </span>           #<span class="hl sym">:</span>component-depends-on
<a id="l_75"></a><span class="hl line">   75 </span>
<a id="l_76"></a><span class="hl line">   76 </span>           #<span class="hl sym">:</span>system-description
<a id="l_77"></a><span class="hl line">   77 </span>           #<span class="hl sym">:</span>system-long-description
<a id="l_78"></a><span class="hl line">   78 </span>           #<span class="hl sym">:</span>system-author
<a id="l_79"></a><span class="hl line">   79 </span>           #<span class="hl sym">:</span>system-maintainer
<a id="l_80"></a><span class="hl line">   80 </span>           #<span class="hl sym">:</span>system-license
<a id="l_81"></a><span class="hl line">   81 </span>           #<span class="hl sym">:</span>system-licence
<a id="l_82"></a><span class="hl line">   82 </span>           #<span class="hl sym">:</span>system-source-file
<a id="l_83"></a><span class="hl line">   83 </span>           #<span class="hl sym">:</span>system-relative-pathname
<a id="l_84"></a><span class="hl line">   84 </span>
<a id="l_85"></a><span class="hl line">   85 </span>           #<span class="hl sym">:</span>operation-on-warnings
<a id="l_86"></a><span class="hl line">   86 </span>           #<span class="hl sym">:</span>operation-on-failure
<a id="l_87"></a><span class="hl line">   87 </span>
<a id="l_88"></a><span class="hl line">   88 </span>           <span class="hl slc">;#:*component-parent-pathname*</span>
<a id="l_89"></a><span class="hl line">   89 </span>           #<span class="hl sym">:*</span>system-definition-search-functions<span class="hl sym">*</span>
<a id="l_90"></a><span class="hl line">   90 </span>           #<span class="hl sym">:*</span>central-registry<span class="hl sym">*</span>         <span class="hl slc">; variables</span>
<a id="l_91"></a><span class="hl line">   91 </span>           #<span class="hl sym">:*</span>compile-file-warnings-behaviour<span class="hl sym">*</span>
<a id="l_92"></a><span class="hl line">   92 </span>           #<span class="hl sym">:*</span>compile-file-failure-behaviour<span class="hl sym">*</span>
<a id="l_93"></a><span class="hl line">   93 </span>           #<span class="hl sym">:*</span>asdf-revision<span class="hl sym">*</span>
<a id="l_94"></a><span class="hl line">   94 </span>
<a id="l_95"></a><span class="hl line">   95 </span>           #<span class="hl sym">:</span>operation-error #<span class="hl sym">:</span>compile-failed #<span class="hl sym">:</span>compile-warned #<span class="hl sym">:</span>compile-error
<a id="l_96"></a><span class="hl line">   96 </span>           #<span class="hl sym">:</span>error-component #<span class="hl sym">:</span>error-operation
<a id="l_97"></a><span class="hl line">   97 </span>           #<span class="hl sym">:</span>system-definition-error
<a id="l_98"></a><span class="hl line">   98 </span>           #<span class="hl sym">:</span>missing-component
<a id="l_99"></a><span class="hl line">   99 </span>           #<span class="hl sym">:</span>missing-dependency
<a id="l_100"></a><span class="hl line">  100 </span>           #<span class="hl sym">:</span>circular-dependency        <span class="hl slc">; errors</span>
<a id="l_101"></a><span class="hl line">  101 </span>           #<span class="hl sym">:</span>duplicate-names
<a id="l_102"></a><span class="hl line">  102 </span>
<a id="l_103"></a><span class="hl line">  103 </span>           #<span class="hl sym">:</span>retry
<a id="l_104"></a><span class="hl line">  104 </span>           #<span class="hl sym">:</span>accept                     <span class="hl slc">; restarts</span>
<a id="l_105"></a><span class="hl line">  105 </span>
<a id="l_106"></a><span class="hl line">  106 </span>           #<span class="hl sym">:</span>preference-file-for-system<span class="hl sym">/</span>operation
<a id="l_107"></a><span class="hl line">  107 </span>           #<span class="hl sym">:</span><span class="hl kwa">load</span>-preferences
<a id="l_108"></a><span class="hl line">  108 </span>           <span class="hl sym">)</span>
<a id="l_109"></a><span class="hl line">  109 </span>  <span class="hl sym">(:</span>use <span class="hl sym">:</span>cl<span class="hl sym">))</span>
<a id="l_110"></a><span class="hl line">  110 </span>
<a id="l_111"></a><span class="hl line">  111 </span>
<a id="l_112"></a><span class="hl line">  112 </span>#<span class="hl sym">+</span>nil
<a id="l_113"></a><span class="hl line">  113 </span><span class="hl sym">(</span>error <span class="hl str">&quot;The author of this file habitually uses #+nil to comment out ~</span>
<a id="l_114"></a><span class="hl line">  114 </span><span class="hl str">        forms. But don't worry, it was unlikely to work in the New ~</span>
<a id="l_115"></a><span class="hl line">  115 </span><span class="hl str">        Implementation of Lisp anyway&quot;</span><span class="hl sym">)</span>
<a id="l_116"></a><span class="hl line">  116 </span>
<a id="l_117"></a><span class="hl line">  117 </span><span class="hl sym">(</span>in-package #<span class="hl sym">:</span>asdf<span class="hl sym">)</span>
<a id="l_118"></a><span class="hl line">  118 </span>
<a id="l_119"></a><span class="hl line">  119 </span><span class="hl sym">(</span>defvar <span class="hl sym">*</span>asdf-revision<span class="hl sym">* (</span>let<span class="hl sym">* ((</span>v <span class="hl str">&quot;$Revision$&quot;</span><span class="hl sym">)</span>
<a id="l_120"></a><span class="hl line">  120 </span>                               <span class="hl sym">(</span>colon <span class="hl sym">(</span><span class="hl kwa">or</span> <span class="hl sym">(</span>position #\: v<span class="hl sym">)</span> -<span class="hl num">1</span><span class="hl sym">))</span>
<a id="l_121"></a><span class="hl line">  121 </span>                               <span class="hl sym">(</span>dot <span class="hl sym">(</span>position #\. v<span class="hl sym">)))</span>
<a id="l_122"></a><span class="hl line">  122 </span>                          <span class="hl sym">(</span><span class="hl kwa">and</span> v colon dot
<a id="l_123"></a><span class="hl line">  123 </span>                               <span class="hl sym">(</span><span class="hl kwa">list</span> <span class="hl sym">(</span>parse-integer v <span class="hl sym">:</span>start <span class="hl sym">(</span><span class="hl num">1</span><span class="hl sym">+</span> colon<span class="hl sym">)</span>
<a id="l_124"></a><span class="hl line">  124 </span>                                                      <span class="hl sym">:</span>junk-allowed t<span class="hl sym">)</span>
<a id="l_125"></a><span class="hl line">  125 </span>                                     <span class="hl sym">(</span>parse-integer v <span class="hl sym">:</span>start <span class="hl sym">(</span><span class="hl num">1</span><span class="hl sym">+</span> dot<span class="hl sym">)</span>
<a id="l_126"></a><span class="hl line">  126 </span>                                                      <span class="hl sym">:</span>junk-allowed t<span class="hl sym">)))))</span>
<a id="l_127"></a><span class="hl line">  127 </span>
<a id="l_128"></a><span class="hl line">  128 </span><span class="hl sym">(</span>defvar <span class="hl sym">*</span>compile-file-warnings-behaviour<span class="hl sym">* :</span>warn<span class="hl sym">)</span>
<a id="l_129"></a><span class="hl line">  129 </span>
<a id="l_130"></a><span class="hl line">  130 </span><span class="hl sym">(</span>defvar <span class="hl sym">*</span>compile-file-failure-behaviour<span class="hl sym">*</span> #<span class="hl sym">+</span>sbcl <span class="hl sym">:</span>error #-sbcl <span class="hl sym">:</span>warn<span class="hl sym">)</span>
<a id="l_131"></a><span class="hl line">  131 </span>
<a id="l_132"></a><span class="hl line">  132 </span><span class="hl sym">(</span>defvar <span class="hl sym">*</span>verbose-out<span class="hl sym">*</span> nil<span class="hl sym">)</span>
<a id="l_133"></a><span class="hl line">  133 </span>
<a id="l_134"></a><span class="hl line">  134 </span><span class="hl sym">(</span>defparameter <span class="hl sym">+</span>asdf-methods<span class="hl sym">+</span>
<a id="l_135"></a><span class="hl line">  135 </span>  <span class="hl sym">'(</span>perform explain output-files operation-done-p<span class="hl sym">))</span>
<a id="l_136"></a><span class="hl line">  136 </span>
<a id="l_137"></a><span class="hl line">  137 </span><span class="hl slc">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</span>
<a id="l_138"></a><span class="hl line">  138 </span><span class="hl slc">;; utility stuff</span>
<a id="l_139"></a><span class="hl line">  139 </span>
<a id="l_140"></a><span class="hl line">  140 </span><span class="hl sym">(</span>defmacro aif <span class="hl sym">(</span>test then <span class="hl sym">&amp;</span>optional else<span class="hl sym">)</span>
<a id="l_141"></a><span class="hl line">  141 </span>  `<span class="hl sym">(</span>let <span class="hl sym">((</span>it <span class="hl sym">,</span>test<span class="hl sym">)) (</span><span class="hl kwa">if</span> it <span class="hl sym">,</span>then <span class="hl sym">,</span>else<span class="hl sym">)))</span>
<a id="l_142"></a><span class="hl line">  142 </span>
<a id="l_143"></a><span class="hl line">  143 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> pathname-sans-name<span class="hl sym">+</span><span class="hl kwa">type</span> <span class="hl sym">(</span>pathname<span class="hl sym">)</span>
<a id="l_144"></a><span class="hl line">  144 </span>  <span class="hl str">&quot;Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,</span>
<a id="l_145"></a><span class="hl line">  145 </span><span class="hl str">and NIL NAME and TYPE components&quot;</span>
<a id="l_146"></a><span class="hl line">  146 </span>  <span class="hl sym">(</span>make-pathname <span class="hl sym">:</span>name nil <span class="hl sym">:</span><span class="hl kwa">type</span> nil <span class="hl sym">:</span>defaults pathname<span class="hl sym">))</span>
<a id="l_147"></a><span class="hl line">  147 </span>
<a id="l_148"></a><span class="hl line">  148 </span><span class="hl sym">(</span>define-modify-macro appendf <span class="hl sym">(&amp;</span>rest args<span class="hl sym">)</span>
<a id="l_149"></a><span class="hl line">  149 </span>  <span class="hl kwa">append</span> <span class="hl str">&quot;Append onto list&quot;</span><span class="hl sym">)</span>
<a id="l_150"></a><span class="hl line">  150 </span>
<a id="l_151"></a><span class="hl line">  151 </span><span class="hl slc">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</span>
<a id="l_152"></a><span class="hl line">  152 </span><span class="hl slc">;; classes, condiitons</span>
<a id="l_153"></a><span class="hl line">  153 </span>
<a id="l_154"></a><span class="hl line">  154 </span><span class="hl sym">(</span>define-condition system-definition-error <span class="hl sym">(</span>error<span class="hl sym">) ()</span>
<a id="l_155"></a><span class="hl line">  155 </span>  <span class="hl slc">;; [this use of :report should be redundant, but unfortunately it's not.</span>
<a id="l_156"></a><span class="hl line">  156 </span>  <span class="hl slc">;; cmucl's lisp::output-instance prefers the kernel:slot-class-print-function</span>
<a id="l_157"></a><span class="hl line">  157 </span>  <span class="hl slc">;; over print-object; this is always conditions::%print-condition for</span>
<a id="l_158"></a><span class="hl line">  158 </span>  <span class="hl slc">;; condition objects, which in turn does inheritance of :report options at</span>
<a id="l_159"></a><span class="hl line">  159 </span>  <span class="hl slc">;; run-time.  fortunately, inheritance means we only need this kludge here in</span>
<a id="l_160"></a><span class="hl line">  160 </span>  <span class="hl slc">;; order to fix all conditions that build on it.  -- rgr, 28-Jul-02.]</span>
<a id="l_161"></a><span class="hl line">  161 </span>  #<span class="hl sym">+</span>cmu <span class="hl sym">(:</span>report <span class="hl kwa">print</span>-object<span class="hl sym">))</span>
<a id="l_162"></a><span class="hl line">  162 </span>
<a id="l_163"></a><span class="hl line">  163 </span><span class="hl sym">(</span>define-condition formatted-system-definition-error <span class="hl sym">(</span>system-definition-error<span class="hl sym">)</span>
<a id="l_164"></a><span class="hl line">  164 </span>  <span class="hl sym">((</span>format-control <span class="hl sym">:</span>initarg <span class="hl sym">:</span>format-control <span class="hl sym">:</span>reader format-control<span class="hl sym">)</span>
<a id="l_165"></a><span class="hl line">  165 </span>   <span class="hl sym">(</span>format-arguments <span class="hl sym">:</span>initarg <span class="hl sym">:</span>format-arguments <span class="hl sym">:</span>reader format-arguments<span class="hl sym">))</span>
<a id="l_166"></a><span class="hl line">  166 </span>  <span class="hl sym">(:</span>report <span class="hl sym">(</span><span class="hl kwa">lambda</span> <span class="hl sym">(</span>c s<span class="hl sym">)</span>
<a id="l_167"></a><span class="hl line">  167 </span>             <span class="hl sym">(</span><span class="hl kwa">apply</span> #<span class="hl sym">'</span>format s <span class="hl sym">(</span>format-control c<span class="hl sym">) (</span>format-arguments c<span class="hl sym">)))))</span>
<a id="l_168"></a><span class="hl line">  168 </span>
<a id="l_169"></a><span class="hl line">  169 </span><span class="hl sym">(</span>define-condition circular-dependency <span class="hl sym">(</span>system-definition-error<span class="hl sym">)</span>
<a id="l_170"></a><span class="hl line">  170 </span>  <span class="hl sym">((</span>components <span class="hl sym">:</span>initarg <span class="hl sym">:</span>components <span class="hl sym">:</span>reader circular-dependency-components<span class="hl sym">)))</span>
<a id="l_171"></a><span class="hl line">  171 </span>
<a id="l_172"></a><span class="hl line">  172 </span><span class="hl sym">(</span>define-condition duplicate-names <span class="hl sym">(</span>system-definition-error<span class="hl sym">)</span>
<a id="l_173"></a><span class="hl line">  173 </span>  <span class="hl sym">((</span>name <span class="hl sym">:</span>initarg <span class="hl sym">:</span>name <span class="hl sym">:</span>reader duplicate-names-name<span class="hl sym">)))</span>
<a id="l_174"></a><span class="hl line">  174 </span>
<a id="l_175"></a><span class="hl line">  175 </span><span class="hl sym">(</span>define-condition missing-component <span class="hl sym">(</span>system-definition-error<span class="hl sym">)</span>
<a id="l_176"></a><span class="hl line">  176 </span>  <span class="hl sym">((</span>requires <span class="hl sym">:</span>initform <span class="hl str">&quot;(unnamed)&quot;</span> <span class="hl sym">:</span>reader missing-requires <span class="hl sym">:</span>initarg <span class="hl sym">:</span>requires<span class="hl sym">)</span>
<a id="l_177"></a><span class="hl line">  177 </span>   <span class="hl sym">(</span>version <span class="hl sym">:</span>initform nil <span class="hl sym">:</span>reader missing-version <span class="hl sym">:</span>initarg <span class="hl sym">:</span>version<span class="hl sym">)</span>
<a id="l_178"></a><span class="hl line">  178 </span>   <span class="hl sym">(</span>parent <span class="hl sym">:</span>initform nil <span class="hl sym">:</span>reader missing-parent <span class="hl sym">:</span>initarg <span class="hl sym">:</span>parent<span class="hl sym">)))</span>
<a id="l_179"></a><span class="hl line">  179 </span>
<a id="l_180"></a><span class="hl line">  180 </span><span class="hl sym">(</span>define-condition missing-dependency <span class="hl sym">(</span>missing-component<span class="hl sym">)</span>
<a id="l_181"></a><span class="hl line">  181 </span>  <span class="hl sym">((</span>required-by <span class="hl sym">:</span>initarg <span class="hl sym">:</span>required-by <span class="hl sym">:</span>reader missing-required-by<span class="hl sym">)))</span>
<a id="l_182"></a><span class="hl line">  182 </span>
<a id="l_183"></a><span class="hl line">  183 </span><span class="hl sym">(</span>define-condition operation-error <span class="hl sym">(</span>error<span class="hl sym">)</span>
<a id="l_184"></a><span class="hl line">  184 </span>  <span class="hl sym">((</span>component <span class="hl sym">:</span>reader error-component <span class="hl sym">:</span>initarg <span class="hl sym">:</span>component<span class="hl sym">)</span>
<a id="l_185"></a><span class="hl line">  185 </span>   <span class="hl sym">(</span>operation <span class="hl sym">:</span>reader error-operation <span class="hl sym">:</span>initarg <span class="hl sym">:</span>operation<span class="hl sym">))</span>
<a id="l_186"></a><span class="hl line">  186 </span>  <span class="hl sym">(:</span>report <span class="hl sym">(</span><span class="hl kwa">lambda</span> <span class="hl sym">(</span>c s<span class="hl sym">)</span>
<a id="l_187"></a><span class="hl line">  187 </span>             <span class="hl sym">(</span>format s <span class="hl str">&quot;~&#64;&lt;erred while invoking ~A on ~A~&#64;:&gt;&quot;</span>
<a id="l_188"></a><span class="hl line">  188 </span>                     <span class="hl sym">(</span>error-operation c<span class="hl sym">) (</span>error-component c<span class="hl sym">)))))</span>
<a id="l_189"></a><span class="hl line">  189 </span><span class="hl sym">(</span>define-condition compile-error <span class="hl sym">(</span>operation-error<span class="hl sym">) ())</span>
<a id="l_190"></a><span class="hl line">  190 </span><span class="hl sym">(</span>define-condition compile-failed <span class="hl sym">(</span>compile-error<span class="hl sym">) ())</span>
<a id="l_191"></a><span class="hl line">  191 </span><span class="hl sym">(</span>define-condition compile-warned <span class="hl sym">(</span>compile-error<span class="hl sym">) ())</span>
<a id="l_192"></a><span class="hl line">  192 </span>
<a id="l_193"></a><span class="hl line">  193 </span><span class="hl sym">(</span>defclass component <span class="hl sym">()</span>
<a id="l_194"></a><span class="hl line">  194 </span>  <span class="hl sym">((</span>name <span class="hl sym">:</span>accessor component-name <span class="hl sym">:</span>initarg <span class="hl sym">:</span>name <span class="hl sym">:</span>documentation
<a id="l_195"></a><span class="hl line">  195 </span>         <span class="hl str">&quot;Component name: designator for a string composed of portable pathname characters&quot;</span><span class="hl sym">)</span>
<a id="l_196"></a><span class="hl line">  196 </span>   <span class="hl sym">(</span>version <span class="hl sym">:</span>accessor component-version <span class="hl sym">:</span>initarg <span class="hl sym">:</span>version<span class="hl sym">)</span>
<a id="l_197"></a><span class="hl line">  197 </span>   <span class="hl sym">(</span>in-order-to <span class="hl sym">:</span>initform nil <span class="hl sym">:</span>initarg <span class="hl sym">:</span>in-order-to<span class="hl sym">)</span>
<a id="l_198"></a><span class="hl line">  198 </span>   <span class="hl slc">;; XXX crap name</span>
<a id="l_199"></a><span class="hl line">  199 </span>   <span class="hl sym">(</span>do-first <span class="hl sym">:</span>initform nil <span class="hl sym">:</span>initarg <span class="hl sym">:</span>do-first<span class="hl sym">)</span>
<a id="l_200"></a><span class="hl line">  200 </span>   <span class="hl slc">;; methods defined using the &quot;inline&quot; style inside a defsystem form:</span>
<a id="l_201"></a><span class="hl line">  201 </span>   <span class="hl slc">;; need to store them somewhere so we can delete them when the system</span>
<a id="l_202"></a><span class="hl line">  202 </span>   <span class="hl slc">;; is re-evaluated</span>
<a id="l_203"></a><span class="hl line">  203 </span>   <span class="hl sym">(</span>inline-methods <span class="hl sym">:</span>accessor component-inline-methods <span class="hl sym">:</span>initform nil<span class="hl sym">)</span>
<a id="l_204"></a><span class="hl line">  204 </span>   <span class="hl sym">(</span>parent <span class="hl sym">:</span>initarg <span class="hl sym">:</span>parent <span class="hl sym">:</span>initform nil <span class="hl sym">:</span>reader component-parent<span class="hl sym">)</span>
<a id="l_205"></a><span class="hl line">  205 </span>   <span class="hl slc">;; no direct accessor for pathname, we do this as a method to allow</span>
<a id="l_206"></a><span class="hl line">  206 </span>   <span class="hl slc">;; it to default in funky ways if not supplied</span>
<a id="l_207"></a><span class="hl line">  207 </span>   <span class="hl sym">(</span>relative-pathname <span class="hl sym">:</span>initarg <span class="hl sym">:</span>pathname<span class="hl sym">)</span>
<a id="l_208"></a><span class="hl line">  208 </span>   <span class="hl sym">(</span>operation-times <span class="hl sym">:</span>initform <span class="hl sym">(</span>make-hash-table <span class="hl sym">)</span>
<a id="l_209"></a><span class="hl line">  209 </span>                    <span class="hl sym">:</span>accessor component-operation-times<span class="hl sym">)</span>
<a id="l_210"></a><span class="hl line">  210 </span>   <span class="hl slc">;; XXX we should provide some atomic interface for updating the</span>
<a id="l_211"></a><span class="hl line">  211 </span>   <span class="hl slc">;; component properties</span>
<a id="l_212"></a><span class="hl line">  212 </span>   <span class="hl sym">(</span>properties <span class="hl sym">:</span>accessor component-properties <span class="hl sym">:</span>initarg <span class="hl sym">:</span>properties
<a id="l_213"></a><span class="hl line">  213 </span>               <span class="hl sym">:</span>initform nil<span class="hl sym">)))</span>
<a id="l_214"></a><span class="hl line">  214 </span>
<a id="l_215"></a><span class="hl line">  215 </span><span class="hl slc">;;;; methods: conditions</span>
<a id="l_216"></a><span class="hl line">  216 </span>
<a id="l_217"></a><span class="hl line">  217 </span><span class="hl sym">(</span>defmethod <span class="hl kwa">print</span>-object <span class="hl sym">((</span>c missing-dependency<span class="hl sym">)</span> s<span class="hl sym">)</span>
<a id="l_218"></a><span class="hl line">  218 </span>  <span class="hl sym">(</span>format s <span class="hl str">&quot;~&#64;&lt;~A, required by ~A~&#64;:&gt;&quot;</span>
<a id="l_219"></a><span class="hl line">  219 </span>          <span class="hl sym">(</span>call-next-method c nil<span class="hl sym">) (</span>missing-required-by c<span class="hl sym">)))</span>
<a id="l_220"></a><span class="hl line">  220 </span>
<a id="l_221"></a><span class="hl line">  221 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> sysdef-error <span class="hl sym">(</span>format <span class="hl sym">&amp;</span>rest arguments<span class="hl sym">)</span>
<a id="l_222"></a><span class="hl line">  222 </span>  <span class="hl sym">(</span>error <span class="hl sym">'</span>formatted-system-definition-error <span class="hl sym">:</span>format-control format <span class="hl sym">:</span>format-arguments arguments<span class="hl sym">))</span>
<a id="l_223"></a><span class="hl line">  223 </span>
<a id="l_224"></a><span class="hl line">  224 </span><span class="hl slc">;;;; methods: components</span>
<a id="l_225"></a><span class="hl line">  225 </span>
<a id="l_226"></a><span class="hl line">  226 </span><span class="hl sym">(</span>defmethod <span class="hl kwa">print</span>-object <span class="hl sym">((</span>c missing-component<span class="hl sym">)</span> s<span class="hl sym">)</span>
<a id="l_227"></a><span class="hl line">  227 </span>  <span class="hl sym">(</span>format s <span class="hl str">&quot;~&#64;&lt;component ~S not found~</span>
<a id="l_228"></a><span class="hl line">  228 </span><span class="hl str">             ~&#64;[ or does not match version ~A~]~</span>
<a id="l_229"></a><span class="hl line">  229 </span><span class="hl str">             ~&#64;[ in ~A~]~&#64;:&gt;&quot;</span>
<a id="l_230"></a><span class="hl line">  230 </span>          <span class="hl sym">(</span>missing-requires c<span class="hl sym">)</span>
<a id="l_231"></a><span class="hl line">  231 </span>          <span class="hl sym">(</span>missing-version c<span class="hl sym">)</span>
<a id="l_232"></a><span class="hl line">  232 </span>          <span class="hl sym">(</span>when <span class="hl sym">(</span>missing-parent c<span class="hl sym">)</span>
<a id="l_233"></a><span class="hl line">  233 </span>            <span class="hl sym">(</span>component-name <span class="hl sym">(</span>missing-parent c<span class="hl sym">)))))</span>
<a id="l_234"></a><span class="hl line">  234 </span>
<a id="l_235"></a><span class="hl line">  235 </span><span class="hl sym">(</span>defgeneric component-system <span class="hl sym">(</span>component<span class="hl sym">)</span>
<a id="l_236"></a><span class="hl line">  236 </span>  <span class="hl sym">(:</span>documentation <span class="hl str">&quot;Find the top-level system containing COMPONENT&quot;</span><span class="hl sym">))</span>
<a id="l_237"></a><span class="hl line">  237 </span>
<a id="l_238"></a><span class="hl line">  238 </span><span class="hl sym">(</span>defmethod component-system <span class="hl sym">((</span>component component<span class="hl sym">))</span>
<a id="l_239"></a><span class="hl line">  239 </span>  <span class="hl sym">(</span>aif <span class="hl sym">(</span>component-parent component<span class="hl sym">)</span>
<a id="l_240"></a><span class="hl line">  240 </span>       <span class="hl sym">(</span>component-system it<span class="hl sym">)</span>
<a id="l_241"></a><span class="hl line">  241 </span>       component<span class="hl sym">))</span>
<a id="l_242"></a><span class="hl line">  242 </span>
<a id="l_243"></a><span class="hl line">  243 </span><span class="hl sym">(</span>defmethod <span class="hl kwa">print</span>-object <span class="hl sym">((</span>c component<span class="hl sym">)</span> stream<span class="hl sym">)</span>
<a id="l_244"></a><span class="hl line">  244 </span>  <span class="hl sym">(</span><span class="hl kwa">print</span>-unreadable-object <span class="hl sym">(</span>c stream <span class="hl sym">:</span><span class="hl kwa">type</span> t <span class="hl sym">:</span>identity t<span class="hl sym">)</span>
<a id="l_245"></a><span class="hl line">  245 </span>    <span class="hl sym">(</span>ignore-errors
<a id="l_246"></a><span class="hl line">  246 </span>      <span class="hl sym">(</span><span class="hl kwa">prin1</span> <span class="hl sym">(</span>component-name c<span class="hl sym">)</span> stream<span class="hl sym">))))</span>
<a id="l_247"></a><span class="hl line">  247 </span>
<a id="l_248"></a><span class="hl line">  248 </span><span class="hl sym">(</span>defclass module <span class="hl sym">(</span>component<span class="hl sym">)</span>
<a id="l_249"></a><span class="hl line">  249 </span>  <span class="hl sym">((</span>components <span class="hl sym">:</span>initform nil <span class="hl sym">:</span>accessor module-components <span class="hl sym">:</span>initarg <span class="hl sym">:</span>components<span class="hl sym">)</span>
<a id="l_250"></a><span class="hl line">  250 </span>   <span class="hl slc">;; what to do if we can't satisfy a dependency of one of this module's</span>
<a id="l_251"></a><span class="hl line">  251 </span>   <span class="hl slc">;; components.  This allows a limited form of conditional processing</span>
<a id="l_252"></a><span class="hl line">  252 </span>   <span class="hl sym">(</span><span class="hl kwa">if</span>-component-dep-fails <span class="hl sym">:</span>initform <span class="hl sym">:</span>fail
<a id="l_253"></a><span class="hl line">  253 </span>                           <span class="hl sym">:</span>accessor module-<span class="hl kwa">if</span>-component-dep-fails
<a id="l_254"></a><span class="hl line">  254 </span>                           <span class="hl sym">:</span>initarg <span class="hl sym">:</span><span class="hl kwa">if</span>-component-dep-fails<span class="hl sym">)</span>
<a id="l_255"></a><span class="hl line">  255 </span>   <span class="hl sym">(</span>default-component-class <span class="hl sym">:</span>accessor module-default-component-class
<a id="l_256"></a><span class="hl line">  256 </span>     <span class="hl sym">:</span>initform <span class="hl sym">'</span>cl-source-file <span class="hl sym">:</span>initarg <span class="hl sym">:</span>default-component-class<span class="hl sym">)))</span>
<a id="l_257"></a><span class="hl line">  257 </span>
<a id="l_258"></a><span class="hl line">  258 </span><span class="hl sym">(</span>defgeneric component-pathname <span class="hl sym">(</span>component<span class="hl sym">)</span>
<a id="l_259"></a><span class="hl line">  259 </span>  <span class="hl sym">(:</span>documentation <span class="hl str">&quot;Extracts the pathname applicable for a particular component.&quot;</span><span class="hl sym">))</span>
<a id="l_260"></a><span class="hl line">  260 </span>
<a id="l_261"></a><span class="hl line">  261 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> component-parent-pathname <span class="hl sym">(</span>component<span class="hl sym">)</span>
<a id="l_262"></a><span class="hl line">  262 </span>  <span class="hl sym">(</span>aif <span class="hl sym">(</span>component-parent component<span class="hl sym">)</span>
<a id="l_263"></a><span class="hl line">  263 </span>       <span class="hl sym">(</span>component-pathname it<span class="hl sym">)</span>
<a id="l_264"></a><span class="hl line">  264 </span>       <span class="hl sym">*</span>default-pathname-defaults<span class="hl sym">*))</span>
<a id="l_265"></a><span class="hl line">  265 </span>
<a id="l_266"></a><span class="hl line">  266 </span><span class="hl sym">(</span>defgeneric component-relative-pathname <span class="hl sym">(</span>component<span class="hl sym">)</span>
<a id="l_267"></a><span class="hl line">  267 </span>  <span class="hl sym">(:</span>documentation <span class="hl str">&quot;Extracts the relative pathname applicable for a particular component.&quot;</span><span class="hl sym">))</span>
<a id="l_268"></a><span class="hl line">  268 </span>
<a id="l_269"></a><span class="hl line">  269 </span><span class="hl sym">(</span>defmethod component-relative-pathname <span class="hl sym">((</span>component module<span class="hl sym">))</span>
<a id="l_270"></a><span class="hl line">  270 </span>  <span class="hl sym">(</span><span class="hl kwa">or</span> <span class="hl sym">(</span>slot-value component <span class="hl sym">'</span>relative-pathname<span class="hl sym">)</span>
<a id="l_271"></a><span class="hl line">  271 </span>      <span class="hl sym">(</span>make-pathname
<a id="l_272"></a><span class="hl line">  272 </span>       <span class="hl sym">:</span>directory `<span class="hl sym">(:</span>relative <span class="hl sym">,(</span>component-name component<span class="hl sym">))</span>
<a id="l_273"></a><span class="hl line">  273 </span>       <span class="hl sym">:</span>host <span class="hl sym">(</span>pathname-host <span class="hl sym">(</span>component-parent-pathname component<span class="hl sym">)))))</span>
<a id="l_274"></a><span class="hl line">  274 </span>
<a id="l_275"></a><span class="hl line">  275 </span><span class="hl sym">(</span>defmethod component-pathname <span class="hl sym">((</span>component component<span class="hl sym">))</span>
<a id="l_276"></a><span class="hl line">  276 </span>  <span class="hl sym">(</span>let <span class="hl sym">((*</span>default-pathname-defaults<span class="hl sym">* (</span>component-parent-pathname component<span class="hl sym">)))</span>
<a id="l_277"></a><span class="hl line">  277 </span>    <span class="hl sym">(</span>merge-pathnames <span class="hl sym">(</span>component-relative-pathname component<span class="hl sym">))))</span>
<a id="l_278"></a><span class="hl line">  278 </span>
<a id="l_279"></a><span class="hl line">  279 </span><span class="hl sym">(</span>defgeneric component-property <span class="hl sym">(</span>component property<span class="hl sym">))</span>
<a id="l_280"></a><span class="hl line">  280 </span>
<a id="l_281"></a><span class="hl line">  281 </span><span class="hl sym">(</span>defmethod component-property <span class="hl sym">((</span>c component<span class="hl sym">)</span> property<span class="hl sym">)</span>
<a id="l_282"></a><span class="hl line">  282 </span>  <span class="hl sym">(</span><span class="hl kwa">cdr</span> <span class="hl sym">(</span><span class="hl kwa">assoc</span> property <span class="hl sym">(</span>slot-value c <span class="hl sym">'</span>properties<span class="hl sym">) :</span>test #<span class="hl sym">'</span><span class="hl kwa">equal</span><span class="hl sym">)))</span>
<a id="l_283"></a><span class="hl line">  283 </span>
<a id="l_284"></a><span class="hl line">  284 </span><span class="hl sym">(</span>defgeneric <span class="hl sym">(</span>setf component-property<span class="hl sym">) (</span>new-value component property<span class="hl sym">))</span>
<a id="l_285"></a><span class="hl line">  285 </span>
<a id="l_286"></a><span class="hl line">  286 </span><span class="hl sym">(</span>defmethod <span class="hl sym">(</span>setf component-property<span class="hl sym">) (</span>new-value <span class="hl sym">(</span>c component<span class="hl sym">)</span> property<span class="hl sym">)</span>
<a id="l_287"></a><span class="hl line">  287 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>a <span class="hl sym">(</span><span class="hl kwa">assoc</span> property <span class="hl sym">(</span>slot-value c <span class="hl sym">'</span>properties<span class="hl sym">) :</span>test #<span class="hl sym">'</span><span class="hl kwa">equal</span><span class="hl sym">)))</span>
<a id="l_288"></a><span class="hl line">  288 </span>    <span class="hl sym">(</span><span class="hl kwa">if</span> a
<a id="l_289"></a><span class="hl line">  289 </span>        <span class="hl sym">(</span>setf <span class="hl sym">(</span><span class="hl kwa">cdr</span> a<span class="hl sym">)</span> new-value<span class="hl sym">)</span>
<a id="l_290"></a><span class="hl line">  290 </span>        <span class="hl sym">(</span>setf <span class="hl sym">(</span>slot-value c <span class="hl sym">'</span>properties<span class="hl sym">)</span>
<a id="l_291"></a><span class="hl line">  291 </span>              <span class="hl sym">(</span>acons property new-value <span class="hl sym">(</span>slot-value c <span class="hl sym">'</span>properties<span class="hl sym">))))))</span>
<a id="l_292"></a><span class="hl line">  292 </span>
<a id="l_293"></a><span class="hl line">  293 </span><span class="hl sym">(</span>defclass system <span class="hl sym">(</span>module<span class="hl sym">)</span>
<a id="l_294"></a><span class="hl line">  294 </span>  <span class="hl sym">((</span>description <span class="hl sym">:</span>accessor system-description <span class="hl sym">:</span>initarg <span class="hl sym">:</span>description<span class="hl sym">)</span>
<a id="l_295"></a><span class="hl line">  295 </span>   <span class="hl sym">(</span>long-description
<a id="l_296"></a><span class="hl line">  296 </span>    <span class="hl sym">:</span>accessor system-long-description <span class="hl sym">:</span>initarg <span class="hl sym">:</span>long-description<span class="hl sym">)</span>
<a id="l_297"></a><span class="hl line">  297 </span>   <span class="hl sym">(</span>author <span class="hl sym">:</span>accessor system-author <span class="hl sym">:</span>initarg <span class="hl sym">:</span>author<span class="hl sym">)</span>
<a id="l_298"></a><span class="hl line">  298 </span>   <span class="hl sym">(</span>maintainer <span class="hl sym">:</span>accessor system-maintainer <span class="hl sym">:</span>initarg <span class="hl sym">:</span>maintainer<span class="hl sym">)</span>
<a id="l_299"></a><span class="hl line">  299 </span>   <span class="hl sym">(</span>licence <span class="hl sym">:</span>accessor system-licence <span class="hl sym">:</span>initarg <span class="hl sym">:</span>licence
<a id="l_300"></a><span class="hl line">  300 </span>            <span class="hl sym">:</span>accessor system-license <span class="hl sym">:</span>initarg <span class="hl sym">:</span>license<span class="hl sym">)))</span>
<a id="l_301"></a><span class="hl line">  301 </span>
<a id="l_302"></a><span class="hl line">  302 </span><span class="hl slc">;;; version-satisfies</span>
<a id="l_303"></a><span class="hl line">  303 </span>
<a id="l_304"></a><span class="hl line">  304 </span><span class="hl slc">;;; with apologies to christophe rhodes ...</span>
<a id="l_305"></a><span class="hl line">  305 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> split <span class="hl sym">(</span>string <span class="hl sym">&amp;</span>optional <span class="hl kwa">max</span> <span class="hl sym">(</span>ws <span class="hl sym">'(</span>#\Space #\Tab<span class="hl sym">)))</span>
<a id="l_306"></a><span class="hl line">  306 </span>  <span class="hl sym">(</span>flet <span class="hl sym">((</span>is-ws <span class="hl sym">(</span>char<span class="hl sym">) (</span>find char ws<span class="hl sym">)))</span>
<a id="l_307"></a><span class="hl line">  307 </span>    <span class="hl sym">(</span>nreverse
<a id="l_308"></a><span class="hl line">  308 </span>     <span class="hl sym">(</span>let <span class="hl sym">((</span><span class="hl kwa">list</span> nil<span class="hl sym">) (</span>start <span class="hl num">0</span><span class="hl sym">) (</span>words <span class="hl num">0</span><span class="hl sym">)</span> end<span class="hl sym">)</span>
<a id="l_309"></a><span class="hl line">  309 </span>       <span class="hl sym">(</span>loop
<a id="l_310"></a><span class="hl line">  310 </span>         <span class="hl sym">(</span>when <span class="hl sym">(</span><span class="hl kwa">and max</span> <span class="hl sym">(&gt;=</span> words <span class="hl sym">(</span><span class="hl num">1</span>- <span class="hl kwa">max</span><span class="hl sym">)))</span>
<a id="l_311"></a><span class="hl line">  311 </span>           <span class="hl sym">(</span>return <span class="hl sym">(</span><span class="hl kwa">cons</span> <span class="hl sym">(</span>subseq string start<span class="hl sym">)</span> <span class="hl kwa">list</span><span class="hl sym">)))</span>
<a id="l_312"></a><span class="hl line">  312 </span>         <span class="hl sym">(</span>setf end <span class="hl sym">(</span>position-<span class="hl kwa">if</span> #<span class="hl sym">'</span>is-ws string <span class="hl sym">:</span>start start<span class="hl sym">))</span>
<a id="l_313"></a><span class="hl line">  313 </span>         <span class="hl sym">(</span>push <span class="hl sym">(</span>subseq string start end<span class="hl sym">)</span> <span class="hl kwa">list</span><span class="hl sym">)</span>
<a id="l_314"></a><span class="hl line">  314 </span>         <span class="hl sym">(</span>incf words<span class="hl sym">)</span>
<a id="l_315"></a><span class="hl line">  315 </span>         <span class="hl sym">(</span>unless end <span class="hl sym">(</span>return <span class="hl kwa">list</span><span class="hl sym">))</span>
<a id="l_316"></a><span class="hl line">  316 </span>         <span class="hl sym">(</span>setf start <span class="hl sym">(</span><span class="hl num">1</span><span class="hl sym">+</span> end<span class="hl sym">)))))))</span>
<a id="l_317"></a><span class="hl line">  317 </span>
<a id="l_318"></a><span class="hl line">  318 </span><span class="hl sym">(</span>defgeneric version-satisfies <span class="hl sym">(</span>component version<span class="hl sym">))</span>
<a id="l_319"></a><span class="hl line">  319 </span>
<a id="l_320"></a><span class="hl line">  320 </span><span class="hl sym">(</span>defmethod version-satisfies <span class="hl sym">((</span>c component<span class="hl sym">)</span> version<span class="hl sym">)</span>
<a id="l_321"></a><span class="hl line">  321 </span>  <span class="hl sym">(</span>unless <span class="hl sym">(</span><span class="hl kwa">and</span> version <span class="hl sym">(</span>slot-<span class="hl kwa">boundp</span> c <span class="hl sym">'</span>version<span class="hl sym">))</span>
<a id="l_322"></a><span class="hl line">  322 </span>    <span class="hl sym">(</span>return-from version-satisfies t<span class="hl sym">))</span>
<a id="l_323"></a><span class="hl line">  323 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>x <span class="hl sym">(</span><span class="hl kwa">mapcar</span> #<span class="hl sym">'</span>parse-integer
<a id="l_324"></a><span class="hl line">  324 </span>                   <span class="hl sym">(</span>split <span class="hl sym">(</span>component-version c<span class="hl sym">)</span> nil <span class="hl sym">'(</span>#\.<span class="hl sym">))))</span>
<a id="l_325"></a><span class="hl line">  325 </span>        <span class="hl sym">(</span>y <span class="hl sym">(</span><span class="hl kwa">mapcar</span> #<span class="hl sym">'</span>parse-integer
<a id="l_326"></a><span class="hl line">  326 </span>                   <span class="hl sym">(</span>split version nil <span class="hl sym">'(</span>#\.<span class="hl sym">)))))</span>
<a id="l_327"></a><span class="hl line">  327 </span>    <span class="hl sym">(</span>labels <span class="hl sym">((</span>bigger <span class="hl sym">(</span>x y<span class="hl sym">)</span>
<a id="l_328"></a><span class="hl line">  328 </span>               <span class="hl sym">(</span><span class="hl kwa">cond</span> <span class="hl sym">((</span><span class="hl kwa">not</span> y<span class="hl sym">)</span> t<span class="hl sym">)</span>
<a id="l_329"></a><span class="hl line">  329 </span>                     <span class="hl sym">((</span><span class="hl kwa">not</span> x<span class="hl sym">)</span> nil<span class="hl sym">)</span>
<a id="l_330"></a><span class="hl line">  330 </span>                     <span class="hl sym">((&gt; (</span><span class="hl kwa">car</span> x<span class="hl sym">) (</span><span class="hl kwa">car</span> y<span class="hl sym">))</span> t<span class="hl sym">)</span>
<a id="l_331"></a><span class="hl line">  331 </span>                     <span class="hl sym">((= (</span><span class="hl kwa">car</span> x<span class="hl sym">) (</span><span class="hl kwa">car</span> y<span class="hl sym">))</span>
<a id="l_332"></a><span class="hl line">  332 </span>                      <span class="hl sym">(</span>bigger <span class="hl sym">(</span><span class="hl kwa">cdr</span> x<span class="hl sym">) (</span><span class="hl kwa">cdr</span> y<span class="hl sym">))))))</span>
<a id="l_333"></a><span class="hl line">  333 </span>      <span class="hl sym">(</span><span class="hl kwa">and</span> <span class="hl sym">(= (</span><span class="hl kwa">car</span> x<span class="hl sym">) (</span><span class="hl kwa">car</span> y<span class="hl sym">))</span>
<a id="l_334"></a><span class="hl line">  334 </span>           <span class="hl sym">(</span><span class="hl kwa">or</span> <span class="hl sym">(</span><span class="hl kwa">not</span> <span class="hl sym">(</span><span class="hl kwa">cdr</span> y<span class="hl sym">)) (</span>bigger <span class="hl sym">(</span><span class="hl kwa">cdr</span> x<span class="hl sym">) (</span><span class="hl kwa">cdr</span> y<span class="hl sym">)))))))</span>
<a id="l_335"></a><span class="hl line">  335 </span>
<a id="l_336"></a><span class="hl line">  336 </span><span class="hl slc">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</span>
<a id="l_337"></a><span class="hl line">  337 </span><span class="hl slc">;;; finding systems</span>
<a id="l_338"></a><span class="hl line">  338 </span>
<a id="l_339"></a><span class="hl line">  339 </span><span class="hl sym">(</span>defvar <span class="hl sym">*</span>defined-systems<span class="hl sym">* (</span>make-hash-table <span class="hl sym">:</span>test <span class="hl sym">'</span><span class="hl kwa">equal</span><span class="hl sym">))</span>
<a id="l_340"></a><span class="hl line">  340 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> coerce-name <span class="hl sym">(</span>name<span class="hl sym">)</span>
<a id="l_341"></a><span class="hl line">  341 </span>  <span class="hl sym">(</span>typecase name
<a id="l_342"></a><span class="hl line">  342 </span>    <span class="hl sym">(</span>component <span class="hl sym">(</span>component-name name<span class="hl sym">))</span>
<a id="l_343"></a><span class="hl line">  343 </span>    <span class="hl sym">(</span>symbol <span class="hl sym">(</span>string-downcase <span class="hl sym">(</span>symbol-name name<span class="hl sym">)))</span>
<a id="l_344"></a><span class="hl line">  344 </span>    <span class="hl sym">(</span>string name<span class="hl sym">)</span>
<a id="l_345"></a><span class="hl line">  345 </span>    <span class="hl sym">(</span>t <span class="hl sym">(</span>sysdef-error <span class="hl str">&quot;~&#64;&lt;invalid component designator ~A~&#64;:&gt;&quot;</span> name<span class="hl sym">))))</span>
<a id="l_346"></a><span class="hl line">  346 </span>
<a id="l_347"></a><span class="hl line">  347 </span><span class="hl slc">;;; for the sake of keeping things reasonably neat, we adopt a</span>
<a id="l_348"></a><span class="hl line">  348 </span><span class="hl slc">;;; convention that functions in this list are prefixed SYSDEF-</span>
<a id="l_349"></a><span class="hl line">  349 </span>
<a id="l_350"></a><span class="hl line">  350 </span><span class="hl sym">(</span>defvar <span class="hl sym">*</span>system-definition-search-functions<span class="hl sym">*</span>
<a id="l_351"></a><span class="hl line">  351 </span>  <span class="hl sym">'(</span>sysdef-central-registry-search<span class="hl sym">))</span>
<a id="l_352"></a><span class="hl line">  352 </span>
<a id="l_353"></a><span class="hl line">  353 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> system-definition-pathname <span class="hl sym">(</span>system<span class="hl sym">)</span>
<a id="l_354"></a><span class="hl line">  354 </span>  <span class="hl sym">(</span>some <span class="hl sym">(</span><span class="hl kwa">lambda</span> <span class="hl sym">(</span>x<span class="hl sym">) (</span>funcall x system<span class="hl sym">))</span>
<a id="l_355"></a><span class="hl line">  355 </span>        <span class="hl sym">*</span>system-definition-search-functions<span class="hl sym">*))</span>
<a id="l_356"></a><span class="hl line">  356 </span>
<a id="l_357"></a><span class="hl line">  357 </span><span class="hl sym">(</span>defvar <span class="hl sym">*</span>central-registry<span class="hl sym">*</span>
<a id="l_358"></a><span class="hl line">  358 </span>  <span class="hl sym">'(*</span>default-pathname-defaults<span class="hl sym">*</span>
<a id="l_359"></a><span class="hl line">  359 </span>    #<span class="hl sym">+</span>nil <span class="hl str">&quot;/home/dan/src/sourceforge/cclan/asdf/systems/&quot;</span>
<a id="l_360"></a><span class="hl line">  360 </span>    #<span class="hl sym">+</span>nil <span class="hl str">&quot;telent:asdf;systems;&quot;</span><span class="hl sym">))</span>
<a id="l_361"></a><span class="hl line">  361 </span>
<a id="l_362"></a><span class="hl line">  362 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> sysdef-central-registry-search <span class="hl sym">(</span>system<span class="hl sym">)</span>
<a id="l_363"></a><span class="hl line">  363 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>name <span class="hl sym">(</span>coerce-name system<span class="hl sym">)))</span>
<a id="l_364"></a><span class="hl line">  364 </span>    <span class="hl sym">(</span>block nil
<a id="l_365"></a><span class="hl line">  365 </span>      <span class="hl sym">(</span>dolist <span class="hl sym">(</span>dir <span class="hl sym">*</span>central-registry<span class="hl sym">*)</span>
<a id="l_366"></a><span class="hl line">  366 </span>        <span class="hl sym">(</span>let<span class="hl sym">* ((</span>defaults <span class="hl sym">(</span><span class="hl kwa">eval</span> dir<span class="hl sym">))</span>
<a id="l_367"></a><span class="hl line">  367 </span>               <span class="hl sym">(</span>file <span class="hl sym">(</span><span class="hl kwa">and</span> defaults
<a id="l_368"></a><span class="hl line">  368 </span>                          <span class="hl sym">(</span>make-pathname
<a id="l_369"></a><span class="hl line">  369 </span>                           <span class="hl sym">:</span>defaults defaults <span class="hl sym">:</span>version <span class="hl sym">:</span>newest
<a id="l_370"></a><span class="hl line">  370 </span>                           <span class="hl sym">:</span>name name <span class="hl sym">:</span><span class="hl kwa">type</span> <span class="hl str">&quot;asd&quot;</span> <span class="hl sym">:</span>case <span class="hl sym">:</span>local<span class="hl sym">))))</span>
<a id="l_371"></a><span class="hl line">  371 </span>          <span class="hl sym">(</span><span class="hl kwa">if</span> <span class="hl sym">(</span><span class="hl kwa">and</span> file <span class="hl sym">(</span>probe-file file<span class="hl sym">))</span>
<a id="l_372"></a><span class="hl line">  372 </span>              <span class="hl sym">(</span>return file<span class="hl sym">)))))))</span>
<a id="l_373"></a><span class="hl line">  373 </span>
<a id="l_374"></a><span class="hl line">  374 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> make-temporary-package <span class="hl sym">()</span>
<a id="l_375"></a><span class="hl line">  375 </span>  <span class="hl sym">(</span>flet <span class="hl sym">((</span>try <span class="hl sym">(</span>counter<span class="hl sym">)</span>
<a id="l_376"></a><span class="hl line">  376 </span>           <span class="hl sym">(</span>ignore-errors
<a id="l_377"></a><span class="hl line">  377 </span>             <span class="hl sym">(</span>make-package <span class="hl sym">(</span>format nil <span class="hl str">&quot;ASDF~D&quot;</span> counter<span class="hl sym">)</span>
<a id="l_378"></a><span class="hl line">  378 </span>                           <span class="hl sym">:</span>use <span class="hl sym">'(:</span>cl <span class="hl sym">:</span>asdf<span class="hl sym">)))))</span>
<a id="l_379"></a><span class="hl line">  379 </span>    <span class="hl sym">(</span>do<span class="hl sym">* ((</span>counter <span class="hl num">0</span> <span class="hl sym">(+</span> counter <span class="hl num">1</span><span class="hl sym">))</span>
<a id="l_380"></a><span class="hl line">  380 </span>          <span class="hl sym">(</span>package <span class="hl sym">(</span>try counter<span class="hl sym">) (</span>try counter<span class="hl sym">)))</span>
<a id="l_381"></a><span class="hl line">  381 </span>         <span class="hl sym">(</span>package package<span class="hl sym">))))</span>
<a id="l_382"></a><span class="hl line">  382 </span>
<a id="l_383"></a><span class="hl line">  383 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> find-system <span class="hl sym">(</span>name <span class="hl sym">&amp;</span>optional <span class="hl sym">(</span>error-p t<span class="hl sym">))</span>
<a id="l_384"></a><span class="hl line">  384 </span>  <span class="hl sym">(</span>let<span class="hl sym">* ((</span>name <span class="hl sym">(</span>coerce-name name<span class="hl sym">))</span>
<a id="l_385"></a><span class="hl line">  385 </span>         <span class="hl sym">(</span>in-memory <span class="hl sym">(</span>gethash name <span class="hl sym">*</span>defined-systems<span class="hl sym">*))</span>
<a id="l_386"></a><span class="hl line">  386 </span>         <span class="hl sym">(</span>on-disk <span class="hl sym">(</span>system-definition-pathname name<span class="hl sym">)))</span>
<a id="l_387"></a><span class="hl line">  387 </span>    <span class="hl sym">(</span>when <span class="hl sym">(</span><span class="hl kwa">and</span> on-disk
<a id="l_388"></a><span class="hl line">  388 </span>               <span class="hl sym">(</span><span class="hl kwa">or</span> <span class="hl sym">(</span><span class="hl kwa">not</span> in-memory<span class="hl sym">)</span>
<a id="l_389"></a><span class="hl line">  389 </span>                   <span class="hl sym">(&lt; (</span><span class="hl kwa">car</span> in-memory<span class="hl sym">) (</span>file-write-date on-disk<span class="hl sym">))))</span>
<a id="l_390"></a><span class="hl line">  390 </span>      <span class="hl sym">(</span>let <span class="hl sym">((</span>package <span class="hl sym">(</span>make-temporary-package<span class="hl sym">)))</span>
<a id="l_391"></a><span class="hl line">  391 </span>        <span class="hl sym">(</span>unwind-protect
<a id="l_392"></a><span class="hl line">  392 </span>             <span class="hl sym">(</span>let <span class="hl sym">((*</span>package<span class="hl sym">*</span> package<span class="hl sym">))</span>
<a id="l_393"></a><span class="hl line">  393 </span>               <span class="hl sym">(</span>format
<a id="l_394"></a><span class="hl line">  394 </span>                <span class="hl sym">*</span>verbose-out<span class="hl sym">*</span>
<a id="l_395"></a><span class="hl line">  395 </span>                <span class="hl str">&quot;~&amp;~&#64;&lt;; ~&#64;;loading system definition from ~A into ~A~&#64;:&gt;~%&quot;</span>
<a id="l_396"></a><span class="hl line">  396 </span>                <span class="hl slc">;; FIXME: This wants to be (ENOUGH-NAMESTRING</span>
<a id="l_397"></a><span class="hl line">  397 </span>                <span class="hl slc">;; ON-DISK), but CMUCL barfs on that.</span>
<a id="l_398"></a><span class="hl line">  398 </span>                on-disk
<a id="l_399"></a><span class="hl line">  399 </span>                <span class="hl sym">*</span>package<span class="hl sym">*)</span>
<a id="l_400"></a><span class="hl line">  400 </span>               <span class="hl sym">(</span><span class="hl kwa">load</span> on-disk<span class="hl sym">))</span>
<a id="l_401"></a><span class="hl line">  401 </span>          <span class="hl sym">(</span>delete-package package<span class="hl sym">))))</span>
<a id="l_402"></a><span class="hl line">  402 </span>    <span class="hl sym">(</span>let <span class="hl sym">((</span>in-memory <span class="hl sym">(</span>gethash name <span class="hl sym">*</span>defined-systems<span class="hl sym">*)))</span>
<a id="l_403"></a><span class="hl line">  403 </span>      <span class="hl sym">(</span><span class="hl kwa">if</span> in-memory
<a id="l_404"></a><span class="hl line">  404 </span>          <span class="hl sym">(</span><span class="hl kwa">progn</span> <span class="hl sym">(</span><span class="hl kwa">if</span> on-disk <span class="hl sym">(</span>setf <span class="hl sym">(</span><span class="hl kwa">car</span> in-memory<span class="hl sym">) (</span>file-write-date on-disk<span class="hl sym">)))</span>
<a id="l_405"></a><span class="hl line">  405 </span>                 <span class="hl sym">(</span><span class="hl kwa">cdr</span> in-memory<span class="hl sym">))</span>
<a id="l_406"></a><span class="hl line">  406 </span>          <span class="hl sym">(</span><span class="hl kwa">if</span> error-p <span class="hl sym">(</span>error <span class="hl sym">'</span>missing-component <span class="hl sym">:</span>requires name<span class="hl sym">))))))</span>
<a id="l_407"></a><span class="hl line">  407 </span>
<a id="l_408"></a><span class="hl line">  408 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> register-system <span class="hl sym">(</span>name system<span class="hl sym">)</span>
<a id="l_409"></a><span class="hl line">  409 </span>  <span class="hl sym">(</span>format <span class="hl sym">*</span>verbose-out<span class="hl sym">*</span> <span class="hl str">&quot;~&amp;~&#64;&lt;; ~&#64;;registering ~A as ~A~&#64;:&gt;~%&quot;</span> system name<span class="hl sym">)</span>
<a id="l_410"></a><span class="hl line">  410 </span>  <span class="hl sym">(</span>setf <span class="hl sym">(</span>gethash <span class="hl sym">(</span>coerce-name  name<span class="hl sym">) *</span>defined-systems<span class="hl sym">*)</span>
<a id="l_411"></a><span class="hl line">  411 </span>        <span class="hl sym">(</span><span class="hl kwa">cons</span> <span class="hl sym">(</span>get-universal-time<span class="hl sym">)</span> system<span class="hl sym">)))</span>
<a id="l_412"></a><span class="hl line">  412 </span>
<a id="l_413"></a><span class="hl line">  413 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> system-registered-p <span class="hl sym">(</span>name<span class="hl sym">)</span>
<a id="l_414"></a><span class="hl line">  414 </span>  <span class="hl sym">(</span>gethash <span class="hl sym">(</span>coerce-name name<span class="hl sym">) *</span>defined-systems<span class="hl sym">*))</span>
<a id="l_415"></a><span class="hl line">  415 </span>
<a id="l_416"></a><span class="hl line">  416 </span><span class="hl slc">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</span>
<a id="l_417"></a><span class="hl line">  417 </span><span class="hl slc">;;; finding components</span>
<a id="l_418"></a><span class="hl line">  418 </span>
<a id="l_419"></a><span class="hl line">  419 </span><span class="hl sym">(</span>defgeneric find-component <span class="hl sym">(</span>module name <span class="hl sym">&amp;</span>optional version<span class="hl sym">)</span>
<a id="l_420"></a><span class="hl line">  420 </span>  <span class="hl sym">(:</span>documentation <span class="hl str">&quot;Finds the component with name NAME present in the</span>
<a id="l_421"></a><span class="hl line">  421 </span><span class="hl str">MODULE module; if MODULE is nil, then the component is assumed to be a</span>
<a id="l_422"></a><span class="hl line">  422 </span><span class="hl str">system.&quot;</span><span class="hl sym">))</span>
<a id="l_423"></a><span class="hl line">  423 </span>
<a id="l_424"></a><span class="hl line">  424 </span><span class="hl sym">(</span>defmethod find-component <span class="hl sym">((</span>module module<span class="hl sym">)</span> name <span class="hl sym">&amp;</span>optional version<span class="hl sym">)</span>
<a id="l_425"></a><span class="hl line">  425 </span>  <span class="hl sym">(</span><span class="hl kwa">if</span> <span class="hl sym">(</span>slot-<span class="hl kwa">boundp</span> module <span class="hl sym">'</span>components<span class="hl sym">)</span>
<a id="l_426"></a><span class="hl line">  426 </span>      <span class="hl sym">(</span>let <span class="hl sym">((</span>m <span class="hl sym">(</span>find name <span class="hl sym">(</span>module-components module<span class="hl sym">)</span>
<a id="l_427"></a><span class="hl line">  427 </span>                     <span class="hl sym">:</span>test #<span class="hl sym">'</span><span class="hl kwa">equal</span> <span class="hl sym">:</span>key #<span class="hl sym">'</span>component-name<span class="hl sym">)))</span>
<a id="l_428"></a><span class="hl line">  428 </span>        <span class="hl sym">(</span><span class="hl kwa">if</span> <span class="hl sym">(</span><span class="hl kwa">and</span> m <span class="hl sym">(</span>version-satisfies m version<span class="hl sym">))</span> m<span class="hl sym">))))</span>
<a id="l_429"></a><span class="hl line">  429 </span>
<a id="l_430"></a><span class="hl line">  430 </span>
<a id="l_431"></a><span class="hl line">  431 </span><span class="hl slc">;;; a component with no parent is a system</span>
<a id="l_432"></a><span class="hl line">  432 </span><span class="hl sym">(</span>defmethod find-component <span class="hl sym">((</span>module <span class="hl sym">(</span>eql nil<span class="hl sym">))</span> name <span class="hl sym">&amp;</span>optional version<span class="hl sym">)</span>
<a id="l_433"></a><span class="hl line">  433 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>m <span class="hl sym">(</span>find-system name nil<span class="hl sym">)))</span>
<a id="l_434"></a><span class="hl line">  434 </span>    <span class="hl sym">(</span><span class="hl kwa">if</span> <span class="hl sym">(</span><span class="hl kwa">and</span> m <span class="hl sym">(</span>version-satisfies m version<span class="hl sym">))</span> m<span class="hl sym">)))</span>
<a id="l_435"></a><span class="hl line">  435 </span>
<a id="l_436"></a><span class="hl line">  436 </span><span class="hl slc">;;; component subclasses</span>
<a id="l_437"></a><span class="hl line">  437 </span>
<a id="l_438"></a><span class="hl line">  438 </span><span class="hl sym">(</span>defclass source-file <span class="hl sym">(</span>component<span class="hl sym">) ())</span>
<a id="l_439"></a><span class="hl line">  439 </span>
<a id="l_440"></a><span class="hl line">  440 </span><span class="hl sym">(</span>defclass cl-source-file <span class="hl sym">(</span>source-file<span class="hl sym">) ())</span>
<a id="l_441"></a><span class="hl line">  441 </span><span class="hl sym">(</span>defclass c-source-file <span class="hl sym">(</span>source-file<span class="hl sym">) ())</span>
<a id="l_442"></a><span class="hl line">  442 </span><span class="hl sym">(</span>defclass java-source-file <span class="hl sym">(</span>source-file<span class="hl sym">) ())</span>
<a id="l_443"></a><span class="hl line">  443 </span><span class="hl sym">(</span>defclass static-file <span class="hl sym">(</span>source-file<span class="hl sym">) ())</span>
<a id="l_444"></a><span class="hl line">  444 </span><span class="hl sym">(</span>defclass doc-file <span class="hl sym">(</span>static-file<span class="hl sym">) ())</span>
<a id="l_445"></a><span class="hl line">  445 </span><span class="hl sym">(</span>defclass html-file <span class="hl sym">(</span>doc-file<span class="hl sym">) ())</span>
<a id="l_446"></a><span class="hl line">  446 </span>
<a id="l_447"></a><span class="hl line">  447 </span><span class="hl sym">(</span>defgeneric source-file-<span class="hl kwa">type</span> <span class="hl sym">(</span>component system<span class="hl sym">))</span>
<a id="l_448"></a><span class="hl line">  448 </span><span class="hl sym">(</span>defmethod source-file-<span class="hl kwa">type</span> <span class="hl sym">((</span>c cl-source-file<span class="hl sym">) (</span>s module<span class="hl sym">))</span> <span class="hl str">&quot;lisp&quot;</span><span class="hl sym">)</span>
<a id="l_449"></a><span class="hl line">  449 </span><span class="hl sym">(</span>defmethod source-file-<span class="hl kwa">type</span> <span class="hl sym">((</span>c c-source-file<span class="hl sym">) (</span>s module<span class="hl sym">))</span> <span class="hl str">&quot;c&quot;</span><span class="hl sym">)</span>
<a id="l_450"></a><span class="hl line">  450 </span><span class="hl sym">(</span>defmethod source-file-<span class="hl kwa">type</span> <span class="hl sym">((</span>c java-source-file<span class="hl sym">) (</span>s module<span class="hl sym">))</span> <span class="hl str">&quot;java&quot;</span><span class="hl sym">)</span>
<a id="l_451"></a><span class="hl line">  451 </span><span class="hl sym">(</span>defmethod source-file-<span class="hl kwa">type</span> <span class="hl sym">((</span>c html-file<span class="hl sym">) (</span>s module<span class="hl sym">))</span> <span class="hl str">&quot;html&quot;</span><span class="hl sym">)</span>
<a id="l_452"></a><span class="hl line">  452 </span><span class="hl sym">(</span>defmethod source-file-<span class="hl kwa">type</span> <span class="hl sym">((</span>c static-file<span class="hl sym">) (</span>s module<span class="hl sym">))</span> nil<span class="hl sym">)</span>
<a id="l_453"></a><span class="hl line">  453 </span>
<a id="l_454"></a><span class="hl line">  454 </span><span class="hl sym">(</span>defmethod component-relative-pathname <span class="hl sym">((</span>component source-file<span class="hl sym">))</span>
<a id="l_455"></a><span class="hl line">  455 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>relative-pathname <span class="hl sym">(</span>slot-value component <span class="hl sym">'</span>relative-pathname<span class="hl sym">)))</span>
<a id="l_456"></a><span class="hl line">  456 </span>    <span class="hl sym">(</span><span class="hl kwa">if</span> relative-pathname
<a id="l_457"></a><span class="hl line">  457 </span>        <span class="hl sym">(</span>merge-pathnames
<a id="l_458"></a><span class="hl line">  458 </span>         relative-pathname
<a id="l_459"></a><span class="hl line">  459 </span>         <span class="hl sym">(</span>make-pathname
<a id="l_460"></a><span class="hl line">  460 </span>          <span class="hl sym">:</span><span class="hl kwa">type</span> <span class="hl sym">(</span>source-file-<span class="hl kwa">type</span> component <span class="hl sym">(</span>component-system component<span class="hl sym">))))</span>
<a id="l_461"></a><span class="hl line">  461 </span>        <span class="hl sym">(</span>let<span class="hl sym">* ((*</span>default-pathname-defaults<span class="hl sym">*</span>
<a id="l_462"></a><span class="hl line">  462 </span>                <span class="hl sym">(</span>component-parent-pathname component<span class="hl sym">))</span>
<a id="l_463"></a><span class="hl line">  463 </span>               <span class="hl sym">(</span>name-<span class="hl kwa">type</span>
<a id="l_464"></a><span class="hl line">  464 </span>                <span class="hl sym">(</span>make-pathname
<a id="l_465"></a><span class="hl line">  465 </span>                 <span class="hl sym">:</span>name <span class="hl sym">(</span>component-name component<span class="hl sym">)</span>
<a id="l_466"></a><span class="hl line">  466 </span>                 <span class="hl sym">:</span><span class="hl kwa">type</span> <span class="hl sym">(</span>source-file-<span class="hl kwa">type</span> component
<a id="l_467"></a><span class="hl line">  467 </span>                                         <span class="hl sym">(</span>component-system component<span class="hl sym">)))))</span>
<a id="l_468"></a><span class="hl line">  468 </span>          name-<span class="hl kwa">type</span><span class="hl sym">))))</span>
<a id="l_469"></a><span class="hl line">  469 </span>
<a id="l_470"></a><span class="hl line">  470 </span><span class="hl slc">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</span>
<a id="l_471"></a><span class="hl line">  471 </span><span class="hl slc">;;; operations</span>
<a id="l_472"></a><span class="hl line">  472 </span>
<a id="l_473"></a><span class="hl line">  473 </span><span class="hl slc">;;; one of these is instantiated whenever (operate ) is called</span>
<a id="l_474"></a><span class="hl line">  474 </span>
<a id="l_475"></a><span class="hl line">  475 </span><span class="hl sym">(</span>defclass operation <span class="hl sym">()</span>
<a id="l_476"></a><span class="hl line">  476 </span>  <span class="hl sym">((</span>forced <span class="hl sym">:</span>initform nil <span class="hl sym">:</span>initarg <span class="hl sym">:</span>force <span class="hl sym">:</span>accessor operation-forced<span class="hl sym">)</span>
<a id="l_477"></a><span class="hl line">  477 </span>   <span class="hl sym">(</span>original-initargs <span class="hl sym">:</span>initform nil <span class="hl sym">:</span>initarg <span class="hl sym">:</span>original-initargs
<a id="l_478"></a><span class="hl line">  478 </span>                      <span class="hl sym">:</span>accessor operation-original-initargs<span class="hl sym">)</span>
<a id="l_479"></a><span class="hl line">  479 </span>   <span class="hl sym">(</span>visited-nodes <span class="hl sym">:</span>initform nil <span class="hl sym">:</span>accessor operation-visited-nodes<span class="hl sym">)</span>
<a id="l_480"></a><span class="hl line">  480 </span>   <span class="hl sym">(</span>visiting-nodes <span class="hl sym">:</span>initform nil <span class="hl sym">:</span>accessor operation-visiting-nodes<span class="hl sym">)</span>
<a id="l_481"></a><span class="hl line">  481 </span>   <span class="hl sym">(</span>parent <span class="hl sym">:</span>initform nil <span class="hl sym">:</span>initarg <span class="hl sym">:</span>parent <span class="hl sym">:</span>accessor operation-parent<span class="hl sym">)))</span>
<a id="l_482"></a><span class="hl line">  482 </span>
<a id="l_483"></a><span class="hl line">  483 </span><span class="hl sym">(</span>defmethod <span class="hl kwa">print</span>-object <span class="hl sym">((</span>o operation<span class="hl sym">)</span> stream<span class="hl sym">)</span>
<a id="l_484"></a><span class="hl line">  484 </span>  <span class="hl sym">(</span><span class="hl kwa">print</span>-unreadable-object <span class="hl sym">(</span>o stream <span class="hl sym">:</span><span class="hl kwa">type</span> t <span class="hl sym">:</span>identity t<span class="hl sym">)</span>
<a id="l_485"></a><span class="hl line">  485 </span>    <span class="hl sym">(</span>ignore-errors
<a id="l_486"></a><span class="hl line">  486 </span>      <span class="hl sym">(</span><span class="hl kwa">prin1</span> <span class="hl sym">(</span>operation-original-initargs o<span class="hl sym">)</span> stream<span class="hl sym">))))</span>
<a id="l_487"></a><span class="hl line">  487 </span>
<a id="l_488"></a><span class="hl line">  488 </span><span class="hl sym">(</span>defmethod shared-initialize <span class="hl sym">:</span>after <span class="hl sym">((</span>operation operation<span class="hl sym">)</span> slot-names
<a id="l_489"></a><span class="hl line">  489 </span>                                     <span class="hl sym">&amp;</span>key force
<a id="l_490"></a><span class="hl line">  490 </span>                                     <span class="hl sym">&amp;</span>allow-other-keys<span class="hl sym">)</span>
<a id="l_491"></a><span class="hl line">  491 </span>  <span class="hl sym">(</span>declare <span class="hl sym">(</span>ignore slot-names force<span class="hl sym">))</span>
<a id="l_492"></a><span class="hl line">  492 </span>  <span class="hl slc">;; empty method to disable initarg validity checking</span>
<a id="l_493"></a><span class="hl line">  493 </span>  <span class="hl sym">)</span>
<a id="l_494"></a><span class="hl line">  494 </span>
<a id="l_495"></a><span class="hl line">  495 </span><span class="hl sym">(</span>defgeneric perform <span class="hl sym">(</span>operation component<span class="hl sym">))</span>
<a id="l_496"></a><span class="hl line">  496 </span><span class="hl sym">(</span>defgeneric operation-done-p <span class="hl sym">(</span>operation component<span class="hl sym">))</span>
<a id="l_497"></a><span class="hl line">  497 </span><span class="hl sym">(</span>defgeneric explain <span class="hl sym">(</span>operation component<span class="hl sym">))</span>
<a id="l_498"></a><span class="hl line">  498 </span><span class="hl sym">(</span>defgeneric output-files <span class="hl sym">(</span>operation component<span class="hl sym">))</span>
<a id="l_499"></a><span class="hl line">  499 </span><span class="hl sym">(</span>defgeneric input-files <span class="hl sym">(</span>operation component<span class="hl sym">))</span>
<a id="l_500"></a><span class="hl line">  500 </span>
<a id="l_501"></a><span class="hl line">  501 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> node-for <span class="hl sym">(</span>o c<span class="hl sym">)</span>
<a id="l_502"></a><span class="hl line">  502 </span>  <span class="hl sym">(</span><span class="hl kwa">cons</span> <span class="hl sym">(</span>class-name <span class="hl sym">(</span>class-of o<span class="hl sym">))</span> c<span class="hl sym">))</span>
<a id="l_503"></a><span class="hl line">  503 </span>
<a id="l_504"></a><span class="hl line">  504 </span><span class="hl sym">(</span>defgeneric operation-ancestor <span class="hl sym">(</span>operation<span class="hl sym">)</span>
<a id="l_505"></a><span class="hl line">  505 </span>  <span class="hl sym">(:</span>documentation
<a id="l_506"></a><span class="hl line">  506 </span>   <span class="hl str">&quot;Recursively chase the operation's parent pointer until we get to</span>
<a id="l_507"></a><span class="hl line">  507 </span><span class="hl str">the head of the tree&quot;</span><span class="hl sym">))</span>
<a id="l_508"></a><span class="hl line">  508 </span>
<a id="l_509"></a><span class="hl line">  509 </span><span class="hl sym">(</span>defmethod operation-ancestor <span class="hl sym">((</span>operation operation<span class="hl sym">))</span>
<a id="l_510"></a><span class="hl line">  510 </span>  <span class="hl sym">(</span>aif <span class="hl sym">(</span>operation-parent operation<span class="hl sym">)</span>
<a id="l_511"></a><span class="hl line">  511 </span>       <span class="hl sym">(</span>operation-ancestor it<span class="hl sym">)</span>
<a id="l_512"></a><span class="hl line">  512 </span>       operation<span class="hl sym">))</span>
<a id="l_513"></a><span class="hl line">  513 </span>
<a id="l_514"></a><span class="hl line">  514 </span>
<a id="l_515"></a><span class="hl line">  515 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> make-sub-operation <span class="hl sym">(</span>c o dep-c dep-o<span class="hl sym">)</span>
<a id="l_516"></a><span class="hl line">  516 </span>  <span class="hl sym">(</span>let<span class="hl sym">* ((</span>args <span class="hl sym">(</span>copy-<span class="hl kwa">list</span> <span class="hl sym">(</span>operation-original-initargs o<span class="hl sym">)))</span>
<a id="l_517"></a><span class="hl line">  517 </span>         <span class="hl sym">(</span>force-p <span class="hl sym">(</span>getf args <span class="hl sym">:</span>force<span class="hl sym">)))</span>
<a id="l_518"></a><span class="hl line">  518 </span>    <span class="hl slc">;; note explicit comparison with T: any other non-NIL force value</span>
<a id="l_519"></a><span class="hl line">  519 </span>    <span class="hl slc">;; (e.g. :recursive) will pass through</span>
<a id="l_520"></a><span class="hl line">  520 </span>    <span class="hl sym">(</span><span class="hl kwa">cond</span> <span class="hl sym">((</span><span class="hl kwa">and</span> <span class="hl sym">(</span><span class="hl kwa">null</span> <span class="hl sym">(</span>component-parent c<span class="hl sym">))</span>
<a id="l_521"></a><span class="hl line">  521 </span>                <span class="hl sym">(</span><span class="hl kwa">null</span> <span class="hl sym">(</span>component-parent dep-c<span class="hl sym">))</span>
<a id="l_522"></a><span class="hl line">  522 </span>                <span class="hl sym">(</span><span class="hl kwa">not</span> <span class="hl sym">(</span>eql c dep-c<span class="hl sym">)))</span>
<a id="l_523"></a><span class="hl line">  523 </span>           <span class="hl sym">(</span>when <span class="hl sym">(</span>eql force-p t<span class="hl sym">)</span>
<a id="l_524"></a><span class="hl line">  524 </span>             <span class="hl sym">(</span>setf <span class="hl sym">(</span>getf args <span class="hl sym">:</span>force<span class="hl sym">)</span> nil<span class="hl sym">))</span>
<a id="l_525"></a><span class="hl line">  525 </span>           <span class="hl sym">(</span><span class="hl kwa">apply</span> #<span class="hl sym">'</span>make-instance dep-o
<a id="l_526"></a><span class="hl line">  526 </span>                  <span class="hl sym">:</span>parent o
<a id="l_527"></a><span class="hl line">  527 </span>                  <span class="hl sym">:</span>original-initargs args args<span class="hl sym">))</span>
<a id="l_528"></a><span class="hl line">  528 </span>          <span class="hl sym">((</span>subtypep <span class="hl sym">(</span><span class="hl kwa">type</span>-of o<span class="hl sym">)</span> dep-o<span class="hl sym">)</span>
<a id="l_529"></a><span class="hl line">  529 </span>           o<span class="hl sym">)</span>
<a id="l_530"></a><span class="hl line">  530 </span>          <span class="hl sym">(</span>t
<a id="l_531"></a><span class="hl line">  531 </span>           <span class="hl sym">(</span><span class="hl kwa">apply</span> #<span class="hl sym">'</span>make-instance dep-o
<a id="l_532"></a><span class="hl line">  532 </span>                  <span class="hl sym">:</span>parent o <span class="hl sym">:</span>original-initargs args args<span class="hl sym">)))))</span>
<a id="l_533"></a><span class="hl line">  533 </span>
<a id="l_534"></a><span class="hl line">  534 </span>
<a id="l_535"></a><span class="hl line">  535 </span><span class="hl sym">(</span>defgeneric visit-component <span class="hl sym">(</span>operation component data<span class="hl sym">))</span>
<a id="l_536"></a><span class="hl line">  536 </span>
<a id="l_537"></a><span class="hl line">  537 </span><span class="hl sym">(</span>defmethod visit-component <span class="hl sym">((</span>o operation<span class="hl sym">) (</span>c component<span class="hl sym">)</span> data<span class="hl sym">)</span>
<a id="l_538"></a><span class="hl line">  538 </span>  <span class="hl sym">(</span>unless <span class="hl sym">(</span>component-visited-p o c<span class="hl sym">)</span>
<a id="l_539"></a><span class="hl line">  539 </span>    <span class="hl sym">(</span>push <span class="hl sym">(</span><span class="hl kwa">cons</span> <span class="hl sym">(</span>node-for o c<span class="hl sym">)</span> data<span class="hl sym">)</span>
<a id="l_540"></a><span class="hl line">  540 </span>          <span class="hl sym">(</span>operation-visited-nodes <span class="hl sym">(</span>operation-ancestor o<span class="hl sym">)))))</span>
<a id="l_541"></a><span class="hl line">  541 </span>
<a id="l_542"></a><span class="hl line">  542 </span><span class="hl sym">(</span>defgeneric component-visited-p <span class="hl sym">(</span>operation component<span class="hl sym">))</span>
<a id="l_543"></a><span class="hl line">  543 </span>
<a id="l_544"></a><span class="hl line">  544 </span><span class="hl sym">(</span>defmethod component-visited-p <span class="hl sym">((</span>o operation<span class="hl sym">) (</span>c component<span class="hl sym">))</span>
<a id="l_545"></a><span class="hl line">  545 </span>  <span class="hl sym">(</span><span class="hl kwa">assoc</span> <span class="hl sym">(</span>node-for o c<span class="hl sym">)</span>
<a id="l_546"></a><span class="hl line">  546 </span>         <span class="hl sym">(</span>operation-visited-nodes <span class="hl sym">(</span>operation-ancestor o<span class="hl sym">))</span>
<a id="l_547"></a><span class="hl line">  547 </span>         <span class="hl sym">:</span>test <span class="hl sym">'</span><span class="hl kwa">equal</span><span class="hl sym">))</span>
<a id="l_548"></a><span class="hl line">  548 </span>
<a id="l_549"></a><span class="hl line">  549 </span><span class="hl sym">(</span>defgeneric <span class="hl sym">(</span>setf visiting-component<span class="hl sym">) (</span>new-value operation component<span class="hl sym">))</span>
<a id="l_550"></a><span class="hl line">  550 </span>
<a id="l_551"></a><span class="hl line">  551 </span><span class="hl sym">(</span>defmethod <span class="hl sym">(</span>setf visiting-component<span class="hl sym">) (</span>new-value operation component<span class="hl sym">)</span>
<a id="l_552"></a><span class="hl line">  552 </span>  <span class="hl slc">;; MCL complains about unused lexical variables</span>
<a id="l_553"></a><span class="hl line">  553 </span>  <span class="hl sym">(</span>declare <span class="hl sym">(</span>ignorable new-value operation component<span class="hl sym">)))</span>
<a id="l_554"></a><span class="hl line">  554 </span>
<a id="l_555"></a><span class="hl line">  555 </span><span class="hl sym">(</span>defmethod <span class="hl sym">(</span>setf visiting-component<span class="hl sym">) (</span>new-value <span class="hl sym">(</span>o operation<span class="hl sym">) (</span>c component<span class="hl sym">))</span>
<a id="l_556"></a><span class="hl line">  556 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>node <span class="hl sym">(</span>node-for o c<span class="hl sym">))</span>
<a id="l_557"></a><span class="hl line">  557 </span>        <span class="hl sym">(</span>a <span class="hl sym">(</span>operation-ancestor o<span class="hl sym">)))</span>
<a id="l_558"></a><span class="hl line">  558 </span>    <span class="hl sym">(</span><span class="hl kwa">if</span> new-value
<a id="l_559"></a><span class="hl line">  559 </span>        <span class="hl sym">(</span>pushnew node <span class="hl sym">(</span>operation-visiting-nodes a<span class="hl sym">) :</span>test <span class="hl sym">'</span><span class="hl kwa">equal</span><span class="hl sym">)</span>
<a id="l_560"></a><span class="hl line">  560 </span>        <span class="hl sym">(</span>setf <span class="hl sym">(</span>operation-visiting-nodes a<span class="hl sym">)</span>
<a id="l_561"></a><span class="hl line">  561 </span>              <span class="hl sym">(</span>remove node  <span class="hl sym">(</span>operation-visiting-nodes a<span class="hl sym">) :</span>test <span class="hl sym">'</span><span class="hl kwa">equal</span><span class="hl sym">)))))</span>
<a id="l_562"></a><span class="hl line">  562 </span>
<a id="l_563"></a><span class="hl line">  563 </span><span class="hl sym">(</span>defgeneric component-visiting-p <span class="hl sym">(</span>operation component<span class="hl sym">))</span>
<a id="l_564"></a><span class="hl line">  564 </span>
<a id="l_565"></a><span class="hl line">  565 </span><span class="hl sym">(</span>defmethod component-visiting-p <span class="hl sym">((</span>o operation<span class="hl sym">) (</span>c component<span class="hl sym">))</span>
<a id="l_566"></a><span class="hl line">  566 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>node <span class="hl sym">(</span><span class="hl kwa">cons</span> o c<span class="hl sym">)))</span>
<a id="l_567"></a><span class="hl line">  567 </span>    <span class="hl sym">(</span><span class="hl kwa">member</span> node <span class="hl sym">(</span>operation-visiting-nodes <span class="hl sym">(</span>operation-ancestor o<span class="hl sym">))</span>
<a id="l_568"></a><span class="hl line">  568 </span>            <span class="hl sym">:</span>test <span class="hl sym">'</span><span class="hl kwa">equal</span><span class="hl sym">)))</span>
<a id="l_569"></a><span class="hl line">  569 </span>
<a id="l_570"></a><span class="hl line">  570 </span><span class="hl sym">(</span>defgeneric component-depends-on <span class="hl sym">(</span>operation component<span class="hl sym">)</span>
<a id="l_571"></a><span class="hl line">  571 </span>  <span class="hl sym">(:</span>documentation
<a id="l_572"></a><span class="hl line">  572 </span>   <span class="hl str">&quot;Returns a list of dependencies needed by the component to perform</span>
<a id="l_573"></a><span class="hl line">  573 </span><span class="hl str">    the operation.  A dependency has one of the following forms:</span>
<a id="l_574"></a><span class="hl line">  574 </span><span class="hl str"></span>
<a id="l_575"></a><span class="hl line">  575 </span><span class="hl str">      (&lt;operation&gt; &lt;component&gt;*), where &lt;operation&gt; is a class</span>
<a id="l_576"></a><span class="hl line">  576 </span><span class="hl str">        designator and each &lt;component&gt; is a component</span>
<a id="l_577"></a><span class="hl line">  577 </span><span class="hl str">        designator, which means that the component depends on</span>
<a id="l_578"></a><span class="hl line">  578 </span><span class="hl str">        &lt;operation&gt; having been performed on each &lt;component&gt;; or</span>
<a id="l_579"></a><span class="hl line">  579 </span><span class="hl str"></span>
<a id="l_580"></a><span class="hl line">  580 </span><span class="hl str">      (FEATURE &lt;feature&gt;), which means that the component depends</span>
<a id="l_581"></a><span class="hl line">  581 </span><span class="hl str">        on &lt;feature&gt;'s presence in *FEATURES*.</span>
<a id="l_582"></a><span class="hl line">  582 </span><span class="hl str"></span>
<a id="l_583"></a><span class="hl line">  583 </span><span class="hl str">    Methods specialized on subclasses of existing component types</span>
<a id="l_584"></a><span class="hl line">  584 </span><span class="hl str">    should usually append the results of CALL-NEXT-METHOD to the</span>
<a id="l_585"></a><span class="hl line">  585 </span><span class="hl str">    list.&quot;</span><span class="hl sym">))</span>
<a id="l_586"></a><span class="hl line">  586 </span>
<a id="l_587"></a><span class="hl line">  587 </span><span class="hl sym">(</span>defmethod component-depends-on <span class="hl sym">((</span>op-spec symbol<span class="hl sym">) (</span>c component<span class="hl sym">))</span>
<a id="l_588"></a><span class="hl line">  588 </span>  <span class="hl sym">(</span>component-depends-on <span class="hl sym">(</span>make-instance op-spec<span class="hl sym">)</span> c<span class="hl sym">))</span>
<a id="l_589"></a><span class="hl line">  589 </span>
<a id="l_590"></a><span class="hl line">  590 </span><span class="hl sym">(</span>defmethod component-depends-on <span class="hl sym">((</span>o operation<span class="hl sym">) (</span>c component<span class="hl sym">))</span>
<a id="l_591"></a><span class="hl line">  591 </span>  <span class="hl sym">(</span><span class="hl kwa">cdr</span> <span class="hl sym">(</span><span class="hl kwa">assoc</span> <span class="hl sym">(</span>class-name <span class="hl sym">(</span>class-of o<span class="hl sym">))</span>
<a id="l_592"></a><span class="hl line">  592 </span>              <span class="hl sym">(</span>slot-value c <span class="hl sym">'</span>in-order-to<span class="hl sym">))))</span>
<a id="l_593"></a><span class="hl line">  593 </span>
<a id="l_594"></a><span class="hl line">  594 </span><span class="hl sym">(</span>defgeneric component-self-dependencies <span class="hl sym">(</span>operation component<span class="hl sym">))</span>
<a id="l_595"></a><span class="hl line">  595 </span>
<a id="l_596"></a><span class="hl line">  596 </span><span class="hl sym">(</span>defmethod component-self-dependencies <span class="hl sym">((</span>o operation<span class="hl sym">) (</span>c component<span class="hl sym">))</span>
<a id="l_597"></a><span class="hl line">  597 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>all-deps <span class="hl sym">(</span>component-depends-on o c<span class="hl sym">)))</span>
<a id="l_598"></a><span class="hl line">  598 </span>    <span class="hl sym">(</span>remove-<span class="hl kwa">if</span>-<span class="hl kwa">not</span> <span class="hl sym">(</span><span class="hl kwa">lambda</span> <span class="hl sym">(</span>x<span class="hl sym">)</span>
<a id="l_599"></a><span class="hl line">  599 </span>                     <span class="hl sym">(</span><span class="hl kwa">member</span> <span class="hl sym">(</span>component-name c<span class="hl sym">) (</span><span class="hl kwa">cdr</span> x<span class="hl sym">) :</span>test #<span class="hl sym">'</span>string<span class="hl sym">=))</span>
<a id="l_600"></a><span class="hl line">  600 </span>                   all-deps<span class="hl sym">)))</span>
<a id="l_601"></a><span class="hl line">  601 </span>
<a id="l_602"></a><span class="hl line">  602 </span><span class="hl sym">(</span>defmethod input-files <span class="hl sym">((</span>operation operation<span class="hl sym">) (</span>c component<span class="hl sym">))</span>
<a id="l_603"></a><span class="hl line">  603 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>parent <span class="hl sym">(</span>component-parent c<span class="hl sym">))</span>
<a id="l_604"></a><span class="hl line">  604 </span>        <span class="hl sym">(</span>self-deps <span class="hl sym">(</span>component-self-dependencies operation c<span class="hl sym">)))</span>
<a id="l_605"></a><span class="hl line">  605 </span>    <span class="hl sym">(</span><span class="hl kwa">if</span> self-deps
<a id="l_606"></a><span class="hl line">  606 </span>        <span class="hl sym">(</span>mapcan <span class="hl sym">(</span><span class="hl kwa">lambda</span> <span class="hl sym">(</span>dep<span class="hl sym">)</span>
<a id="l_607"></a><span class="hl line">  607 </span>                  <span class="hl sym">(</span>destructuring-bind <span class="hl sym">(</span>op name<span class="hl sym">)</span> dep
<a id="l_608"></a><span class="hl line">  608 </span>                    <span class="hl sym">(</span>output-files <span class="hl sym">(</span>make-instance op<span class="hl sym">)</span>
<a id="l_609"></a><span class="hl line">  609 </span>                                  <span class="hl sym">(</span>find-component parent name<span class="hl sym">))))</span>
<a id="l_610"></a><span class="hl line">  610 </span>                self-deps<span class="hl sym">)</span>
<a id="l_611"></a><span class="hl line">  611 </span>        <span class="hl slc">;; no previous operations needed?  I guess we work with the</span>
<a id="l_612"></a><span class="hl line">  612 </span>        <span class="hl slc">;; original source file, then</span>
<a id="l_613"></a><span class="hl line">  613 </span>        <span class="hl sym">(</span><span class="hl kwa">list</span> <span class="hl sym">(</span>component-pathname c<span class="hl sym">)))))</span>
<a id="l_614"></a><span class="hl line">  614 </span>
<a id="l_615"></a><span class="hl line">  615 </span><span class="hl sym">(</span>defmethod input-files <span class="hl sym">((</span>operation operation<span class="hl sym">) (</span>c module<span class="hl sym">))</span> nil<span class="hl sym">)</span>
<a id="l_616"></a><span class="hl line">  616 </span>
<a id="l_617"></a><span class="hl line">  617 </span><span class="hl sym">(</span>defmethod operation-done-p <span class="hl sym">((</span>o operation<span class="hl sym">) (</span>c component<span class="hl sym">))</span>
<a id="l_618"></a><span class="hl line">  618 </span>  <span class="hl sym">(</span>flet <span class="hl sym">((</span>fwd-<span class="hl kwa">or</span>-return-t <span class="hl sym">(</span>file<span class="hl sym">)</span>
<a id="l_619"></a><span class="hl line">  619 </span>           <span class="hl slc">;; if FILE-WRITE-DATE returns NIL, it's possible that the</span>
<a id="l_620"></a><span class="hl line">  620 </span>           <span class="hl slc">;; user or some other agent has deleted an input file.  If</span>
<a id="l_621"></a><span class="hl line">  621 </span>           <span class="hl slc">;; that's the case, well, that's not good, but as long as</span>
<a id="l_622"></a><span class="hl line">  622 </span>           <span class="hl slc">;; the operation is otherwise considered to be done we</span>
<a id="l_623"></a><span class="hl line">  623 </span>           <span class="hl slc">;; could continue and survive.</span>
<a id="l_624"></a><span class="hl line">  624 </span>           <span class="hl sym">(</span>let <span class="hl sym">((</span>date <span class="hl sym">(</span>file-write-date file<span class="hl sym">)))</span>
<a id="l_625"></a><span class="hl line">  625 </span>             <span class="hl sym">(</span><span class="hl kwa">cond</span>
<a id="l_626"></a><span class="hl line">  626 </span>               <span class="hl sym">(</span>date<span class="hl sym">)</span>
<a id="l_627"></a><span class="hl line">  627 </span>               <span class="hl sym">(</span>t
<a id="l_628"></a><span class="hl line">  628 </span>                <span class="hl sym">(</span>warn <span class="hl str">&quot;~&#64;&lt;Missing FILE-WRITE-DATE for ~S: treating ~</span>
<a id="l_629"></a><span class="hl line">  629 </span><span class="hl str">                       operation ~S on component ~S as done.~&#64;:&gt;&quot;</span>
<a id="l_630"></a><span class="hl line">  630 </span>                      file o c<span class="hl sym">)</span>
<a id="l_631"></a><span class="hl line">  631 </span>                <span class="hl sym">(</span>return-from operation-done-p t<span class="hl sym">))))))</span>
<a id="l_632"></a><span class="hl line">  632 </span>    <span class="hl sym">(</span>let <span class="hl sym">((</span>out-files <span class="hl sym">(</span>output-files o c<span class="hl sym">))</span>
<a id="l_633"></a><span class="hl line">  633 </span>          <span class="hl sym">(</span>in-files <span class="hl sym">(</span>input-files o c<span class="hl sym">)))</span>
<a id="l_634"></a><span class="hl line">  634 </span>      <span class="hl sym">(</span><span class="hl kwa">cond</span> <span class="hl sym">((</span><span class="hl kwa">and</span> <span class="hl sym">(</span><span class="hl kwa">not</span> in-files<span class="hl sym">) (</span><span class="hl kwa">not</span> out-files<span class="hl sym">))</span>
<a id="l_635"></a><span class="hl line">  635 </span>             <span class="hl slc">;; arbitrary decision: an operation that uses nothing to</span>
<a id="l_636"></a><span class="hl line">  636 </span>             <span class="hl slc">;; produce nothing probably isn't doing much</span>
<a id="l_637"></a><span class="hl line">  637 </span>             t<span class="hl sym">)</span>
<a id="l_638"></a><span class="hl line">  638 </span>            <span class="hl sym">((</span><span class="hl kwa">not</span> out-files<span class="hl sym">)</span>
<a id="l_639"></a><span class="hl line">  639 </span>             <span class="hl sym">(</span>let <span class="hl sym">((</span>op-done
<a id="l_640"></a><span class="hl line">  640 </span>                    <span class="hl sym">(</span>gethash <span class="hl sym">(</span><span class="hl kwa">type</span>-of o<span class="hl sym">)</span>
<a id="l_641"></a><span class="hl line">  641 </span>                             <span class="hl sym">(</span>component-operation-times c<span class="hl sym">))))</span>
<a id="l_642"></a><span class="hl line">  642 </span>               <span class="hl sym">(</span><span class="hl kwa">and</span> op-done
<a id="l_643"></a><span class="hl line">  643 </span>                    <span class="hl sym">(&gt;=</span> op-done
<a id="l_644"></a><span class="hl line">  644 </span>                        <span class="hl sym">(</span><span class="hl kwa">apply</span> #<span class="hl sym">'</span><span class="hl kwa">max</span>
<a id="l_645"></a><span class="hl line">  645 </span>                               <span class="hl sym">(</span><span class="hl kwa">mapcar</span> #<span class="hl sym">'</span>fwd-<span class="hl kwa">or</span>-return-t in-files<span class="hl sym">))))))</span>
<a id="l_646"></a><span class="hl line">  646 </span>            <span class="hl sym">((</span><span class="hl kwa">not</span> in-files<span class="hl sym">)</span> nil<span class="hl sym">)</span>
<a id="l_647"></a><span class="hl line">  647 </span>            <span class="hl sym">(</span>t
<a id="l_648"></a><span class="hl line">  648 </span>             <span class="hl sym">(</span><span class="hl kwa">and</span>
<a id="l_649"></a><span class="hl line">  649 </span>              <span class="hl sym">(</span>every #<span class="hl sym">'</span>probe-file out-files<span class="hl sym">)</span>
<a id="l_650"></a><span class="hl line">  650 </span>              <span class="hl sym">(&gt; (</span><span class="hl kwa">apply</span> #<span class="hl sym">'</span><span class="hl kwa">min</span> <span class="hl sym">(</span><span class="hl kwa">mapcar</span> #<span class="hl sym">'</span>file-write-date out-files<span class="hl sym">))</span>
<a id="l_651"></a><span class="hl line">  651 </span>                 <span class="hl sym">(</span><span class="hl kwa">apply</span> #<span class="hl sym">'</span><span class="hl kwa">max</span> <span class="hl sym">(</span><span class="hl kwa">mapcar</span> #<span class="hl sym">'</span>fwd-<span class="hl kwa">or</span>-return-t in-files<span class="hl sym">)))))))))</span>
<a id="l_652"></a><span class="hl line">  652 </span>
<a id="l_653"></a><span class="hl line">  653 </span><span class="hl slc">;;; So you look at this code and think &quot;why isn't it a bunch of</span>
<a id="l_654"></a><span class="hl line">  654 </span><span class="hl slc">;;; methods&quot;.  And the answer is, because standard method combination</span>
<a id="l_655"></a><span class="hl line">  655 </span><span class="hl slc">;;; runs :before methods most-&gt;least-specific, which is back to front</span>
<a id="l_656"></a><span class="hl line">  656 </span><span class="hl slc">;;; for our purposes.  And CLISP doesn't have non-standard method</span>
<a id="l_657"></a><span class="hl line">  657 </span><span class="hl slc">;;; combinations, so let's keep it simple and aspire to portability</span>
<a id="l_658"></a><span class="hl line">  658 </span>
<a id="l_659"></a><span class="hl line">  659 </span><span class="hl sym">(</span>defgeneric traverse <span class="hl sym">(</span>operation component<span class="hl sym">))</span>
<a id="l_660"></a><span class="hl line">  660 </span><span class="hl sym">(</span>defmethod traverse <span class="hl sym">((</span>operation operation<span class="hl sym">) (</span>c component<span class="hl sym">))</span>
<a id="l_661"></a><span class="hl line">  661 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>forced nil<span class="hl sym">))</span>
<a id="l_662"></a><span class="hl line">  662 </span>    <span class="hl sym">(</span>labels <span class="hl sym">((</span>do-one-dep <span class="hl sym">(</span>required-op required-c required-v<span class="hl sym">)</span>
<a id="l_663"></a><span class="hl line">  663 </span>               <span class="hl sym">(</span>let<span class="hl sym">* ((</span>dep-c <span class="hl sym">(</span><span class="hl kwa">or</span> <span class="hl sym">(</span>find-component
<a id="l_664"></a><span class="hl line">  664 </span>                                  <span class="hl sym">(</span>component-parent c<span class="hl sym">)</span>
<a id="l_665"></a><span class="hl line">  665 </span>                                  <span class="hl slc">;; XXX tacky.  really we should build the</span>
<a id="l_666"></a><span class="hl line">  666 </span>                                  <span class="hl slc">;; in-order-to slot with canonicalized</span>
<a id="l_667"></a><span class="hl line">  667 </span>                                  <span class="hl slc">;; names instead of coercing this late</span>
<a id="l_668"></a><span class="hl line">  668 </span>                                  <span class="hl sym">(</span>coerce-name required-c<span class="hl sym">)</span> required-v<span class="hl sym">)</span>
<a id="l_669"></a><span class="hl line">  669 </span>                                 <span class="hl sym">(</span>error <span class="hl sym">'</span>missing-dependency
<a id="l_670"></a><span class="hl line">  670 </span>                                        <span class="hl sym">:</span>required-by c
<a id="l_671"></a><span class="hl line">  671 </span>                                        <span class="hl sym">:</span>version required-v
<a id="l_672"></a><span class="hl line">  672 </span>                                        <span class="hl sym">:</span>requires required-c<span class="hl sym">)))</span>
<a id="l_673"></a><span class="hl line">  673 </span>                      <span class="hl sym">(</span>op <span class="hl sym">(</span>make-sub-operation c operation dep-c required-op<span class="hl sym">)))</span>
<a id="l_674"></a><span class="hl line">  674 </span>                 <span class="hl sym">(</span>traverse op dep-c<span class="hl sym">)))</span>
<a id="l_675"></a><span class="hl line">  675 </span>             <span class="hl sym">(</span>do-dep <span class="hl sym">(</span>op dep<span class="hl sym">)</span>
<a id="l_676"></a><span class="hl line">  676 </span>               <span class="hl sym">(</span><span class="hl kwa">cond</span> <span class="hl sym">((</span><span class="hl kwa">eq</span> op <span class="hl sym">'</span>feature<span class="hl sym">)</span>
<a id="l_677"></a><span class="hl line">  677 </span>                      <span class="hl sym">(</span><span class="hl kwa">or</span> <span class="hl sym">(</span><span class="hl kwa">member</span> <span class="hl sym">(</span><span class="hl kwa">car</span> dep<span class="hl sym">) *</span>features<span class="hl sym">*)</span>
<a id="l_678"></a><span class="hl line">  678 </span>                          <span class="hl sym">(</span>error <span class="hl sym">'</span>missing-dependency
<a id="l_679"></a><span class="hl line">  679 </span>                                 <span class="hl sym">:</span>required-by c
<a id="l_680"></a><span class="hl line">  680 </span>                                 <span class="hl sym">:</span>requires <span class="hl sym">(</span><span class="hl kwa">car</span> dep<span class="hl sym">)</span>
<a id="l_681"></a><span class="hl line">  681 </span>                                 <span class="hl sym">:</span>version nil<span class="hl sym">)))</span>
<a id="l_682"></a><span class="hl line">  682 </span>                     <span class="hl sym">(</span>t
<a id="l_683"></a><span class="hl line">  683 </span>                      <span class="hl sym">(</span>dolist <span class="hl sym">(</span>d dep<span class="hl sym">)</span>
<a id="l_684"></a><span class="hl line">  684 </span>                        <span class="hl sym">(</span><span class="hl kwa">cond</span> <span class="hl sym">((</span>consp d<span class="hl sym">)</span>
<a id="l_685"></a><span class="hl line">  685 </span>                               <span class="hl sym">(</span>assert <span class="hl sym">(</span>string-<span class="hl kwa">equal</span>
<a id="l_686"></a><span class="hl line">  686 </span>                                        <span class="hl sym">(</span>symbol-name <span class="hl sym">(</span>first d<span class="hl sym">))</span>
<a id="l_687"></a><span class="hl line">  687 </span>                                        <span class="hl str">&quot;VERSION&quot;</span><span class="hl sym">))</span>
<a id="l_688"></a><span class="hl line">  688 </span>                               <span class="hl sym">(</span>appendf forced
<a id="l_689"></a><span class="hl line">  689 </span>                                        <span class="hl sym">(</span>do-one-dep op <span class="hl sym">(</span>second d<span class="hl sym">) (</span>third d<span class="hl sym">))))</span>
<a id="l_690"></a><span class="hl line">  690 </span>                              <span class="hl sym">(</span>t
<a id="l_691"></a><span class="hl line">  691 </span>                               <span class="hl sym">(</span>appendf forced <span class="hl sym">(</span>do-one-dep op d nil<span class="hl sym">)))))))))</span>
<a id="l_692"></a><span class="hl line">  692 </span>      <span class="hl sym">(</span>aif <span class="hl sym">(</span>component-visited-p operation c<span class="hl sym">)</span>
<a id="l_693"></a><span class="hl line">  693 </span>           <span class="hl sym">(</span>return-from traverse
<a id="l_694"></a><span class="hl line">  694 </span>             <span class="hl sym">(</span><span class="hl kwa">if</span> <span class="hl sym">(</span><span class="hl kwa">cdr</span> it<span class="hl sym">) (</span><span class="hl kwa">list</span> <span class="hl sym">(</span><span class="hl kwa">cons</span> <span class="hl sym">'</span>pruned-op c<span class="hl sym">))</span> nil<span class="hl sym">)))</span>
<a id="l_695"></a><span class="hl line">  695 </span>      <span class="hl slc">;; dependencies</span>
<a id="l_696"></a><span class="hl line">  696 </span>      <span class="hl sym">(</span><span class="hl kwa">if</span> <span class="hl sym">(</span>component-visiting-p operation c<span class="hl sym">)</span>
<a id="l_697"></a><span class="hl line">  697 </span>          <span class="hl sym">(</span>error <span class="hl sym">'</span>circular-dependency <span class="hl sym">:</span>components <span class="hl sym">(</span><span class="hl kwa">list</span> c<span class="hl sym">)))</span>
<a id="l_698"></a><span class="hl line">  698 </span>      <span class="hl sym">(</span>setf <span class="hl sym">(</span>visiting-component operation c<span class="hl sym">)</span> t<span class="hl sym">)</span>
<a id="l_699"></a><span class="hl line">  699 </span>      <span class="hl sym">(</span>loop for <span class="hl sym">(</span>required-op . deps<span class="hl sym">)</span> in <span class="hl sym">(</span>component-depends-on operation c<span class="hl sym">)</span>
<a id="l_700"></a><span class="hl line">  700 </span>            do <span class="hl sym">(</span>do-dep required-op deps<span class="hl sym">))</span>
<a id="l_701"></a><span class="hl line">  701 </span>      <span class="hl slc">;; constituent bits</span>
<a id="l_702"></a><span class="hl line">  702 </span>      <span class="hl sym">(</span>let <span class="hl sym">((</span>module-ops
<a id="l_703"></a><span class="hl line">  703 </span>             <span class="hl sym">(</span>when <span class="hl sym">(</span>typep c <span class="hl sym">'</span>module<span class="hl sym">)</span>
<a id="l_704"></a><span class="hl line">  704 </span>               <span class="hl sym">(</span>let <span class="hl sym">((</span>at-least-one nil<span class="hl sym">)</span>
<a id="l_705"></a><span class="hl line">  705 </span>                     <span class="hl sym">(</span>forced nil<span class="hl sym">)</span>
<a id="l_706"></a><span class="hl line">  706 </span>                     <span class="hl sym">(</span>error nil<span class="hl sym">))</span>
<a id="l_707"></a><span class="hl line">  707 </span>                 <span class="hl sym">(</span>loop for kid in <span class="hl sym">(</span>module-components c<span class="hl sym">)</span>
<a id="l_708"></a><span class="hl line">  708 </span>                       do <span class="hl sym">(</span>handler-case
<a id="l_709"></a><span class="hl line">  709 </span>                              <span class="hl sym">(</span>appendf forced <span class="hl sym">(</span>traverse operation kid <span class="hl sym">))</span>
<a id="l_710"></a><span class="hl line">  710 </span>                            <span class="hl sym">(</span>missing-dependency <span class="hl sym">(</span>condition<span class="hl sym">)</span>
<a id="l_711"></a><span class="hl line">  711 </span>                              <span class="hl sym">(</span><span class="hl kwa">if</span> <span class="hl sym">(</span><span class="hl kwa">eq</span> <span class="hl sym">(</span>module-<span class="hl kwa">if</span>-component-dep-fails c<span class="hl sym">) :</span>fail<span class="hl sym">)</span>
<a id="l_712"></a><span class="hl line">  712 </span>                                  <span class="hl sym">(</span>error condition<span class="hl sym">))</span>
<a id="l_713"></a><span class="hl line">  713 </span>                              <span class="hl sym">(</span>setf error condition<span class="hl sym">))</span>
<a id="l_714"></a><span class="hl line">  714 </span>                            <span class="hl sym">(:</span>no-error <span class="hl sym">(</span>c<span class="hl sym">)</span>
<a id="l_715"></a><span class="hl line">  715 </span>                              <span class="hl sym">(</span>declare <span class="hl sym">(</span>ignore c<span class="hl sym">))</span>
<a id="l_716"></a><span class="hl line">  716 </span>                              <span class="hl sym">(</span>setf at-least-one t<span class="hl sym">))))</span>
<a id="l_717"></a><span class="hl line">  717 </span>                 <span class="hl sym">(</span>when <span class="hl sym">(</span><span class="hl kwa">and</span> <span class="hl sym">(</span><span class="hl kwa">eq</span> <span class="hl sym">(</span>module-<span class="hl kwa">if</span>-component-dep-fails c<span class="hl sym">) :</span>try-next<span class="hl sym">)</span>
<a id="l_718"></a><span class="hl line">  718 </span>                            <span class="hl sym">(</span><span class="hl kwa">not</span> at-least-one<span class="hl sym">))</span>
<a id="l_719"></a><span class="hl line">  719 </span>                   <span class="hl sym">(</span>error error<span class="hl sym">))</span>
<a id="l_720"></a><span class="hl line">  720 </span>                 forced<span class="hl sym">))))</span>
<a id="l_721"></a><span class="hl line">  721 </span>        <span class="hl slc">;; now the thing itself</span>
<a id="l_722"></a><span class="hl line">  722 </span>        <span class="hl sym">(</span>when <span class="hl sym">(</span><span class="hl kwa">or</span> forced module-ops
<a id="l_723"></a><span class="hl line">  723 </span>                  <span class="hl sym">(</span><span class="hl kwa">not</span> <span class="hl sym">(</span>operation-done-p operation c<span class="hl sym">))</span>
<a id="l_724"></a><span class="hl line">  724 </span>                  <span class="hl sym">(</span>let <span class="hl sym">((</span>f <span class="hl sym">(</span>operation-forced <span class="hl sym">(</span>operation-ancestor operation<span class="hl sym">))))</span>
<a id="l_725"></a><span class="hl line">  725 </span>                    <span class="hl sym">(</span><span class="hl kwa">and</span> f <span class="hl sym">(</span><span class="hl kwa">or</span> <span class="hl sym">(</span><span class="hl kwa">not</span> <span class="hl sym">(</span>consp f<span class="hl sym">))</span>
<a id="l_726"></a><span class="hl line">  726 </span>                               <span class="hl sym">(</span><span class="hl kwa">member</span> <span class="hl sym">(</span>component-name
<a id="l_727"></a><span class="hl line">  727 </span>                                        <span class="hl sym">(</span>operation-ancestor operation<span class="hl sym">))</span>
<a id="l_728"></a><span class="hl line">  728 </span>                                       <span class="hl sym">(</span><span class="hl kwa">mapcar</span> #<span class="hl sym">'</span>coerce-name f<span class="hl sym">)</span>
<a id="l_729"></a><span class="hl line">  729 </span>                                       <span class="hl sym">:</span>test #<span class="hl sym">'</span>string<span class="hl sym">=)))))</span>
<a id="l_730"></a><span class="hl line">  730 </span>          <span class="hl sym">(</span>let <span class="hl sym">((</span>do-first <span class="hl sym">(</span><span class="hl kwa">cdr</span> <span class="hl sym">(</span><span class="hl kwa">assoc</span> <span class="hl sym">(</span>class-name <span class="hl sym">(</span>class-of operation<span class="hl sym">))</span>
<a id="l_731"></a><span class="hl line">  731 </span>                                      <span class="hl sym">(</span>slot-value c <span class="hl sym">'</span>do-first<span class="hl sym">)))))</span>
<a id="l_732"></a><span class="hl line">  732 </span>            <span class="hl sym">(</span>loop for <span class="hl sym">(</span>required-op . deps<span class="hl sym">)</span> in do-first
<a id="l_733"></a><span class="hl line">  733 </span>                  do <span class="hl sym">(</span>do-dep required-op deps<span class="hl sym">)))</span>
<a id="l_734"></a><span class="hl line">  734 </span>          <span class="hl sym">(</span>setf forced <span class="hl sym">(</span><span class="hl kwa">append</span> <span class="hl sym">(</span>delete <span class="hl sym">'</span>pruned-op forced <span class="hl sym">:</span>key #<span class="hl sym">'</span><span class="hl kwa">car</span><span class="hl sym">)</span>
<a id="l_735"></a><span class="hl line">  735 </span>                               <span class="hl sym">(</span>delete <span class="hl sym">'</span>pruned-op module-ops <span class="hl sym">:</span>key #<span class="hl sym">'</span><span class="hl kwa">car</span><span class="hl sym">)</span>
<a id="l_736"></a><span class="hl line">  736 </span>                               <span class="hl sym">(</span><span class="hl kwa">list</span> <span class="hl sym">(</span><span class="hl kwa">cons</span> operation c<span class="hl sym">))))))</span>
<a id="l_737"></a><span class="hl line">  737 </span>      <span class="hl sym">(</span>setf <span class="hl sym">(</span>visiting-component operation c<span class="hl sym">)</span> nil<span class="hl sym">)</span>
<a id="l_738"></a><span class="hl line">  738 </span>      <span class="hl sym">(</span>visit-component operation c <span class="hl sym">(</span><span class="hl kwa">and</span> forced t<span class="hl sym">))</span>
<a id="l_739"></a><span class="hl line">  739 </span>      forced<span class="hl sym">)))</span>
<a id="l_740"></a><span class="hl line">  740 </span>
<a id="l_741"></a><span class="hl line">  741 </span>
<a id="l_742"></a><span class="hl line">  742 </span><span class="hl sym">(</span>defmethod perform <span class="hl sym">((</span>operation operation<span class="hl sym">) (</span>c source-file<span class="hl sym">))</span>
<a id="l_743"></a><span class="hl line">  743 </span>  <span class="hl sym">(</span>sysdef-error
<a id="l_744"></a><span class="hl line">  744 </span>   <span class="hl str">&quot;~&#64;&lt;required method PERFORM not implemented ~</span>
<a id="l_745"></a><span class="hl line">  745 </span><span class="hl str">    for operation ~A, component ~A~&#64;:&gt;&quot;</span>
<a id="l_746"></a><span class="hl line">  746 </span>   <span class="hl sym">(</span>class-of operation<span class="hl sym">) (</span>class-of c<span class="hl sym">)))</span>
<a id="l_747"></a><span class="hl line">  747 </span>
<a id="l_748"></a><span class="hl line">  748 </span><span class="hl sym">(</span>defmethod perform <span class="hl sym">((</span>operation operation<span class="hl sym">) (</span>c module<span class="hl sym">))</span>
<a id="l_749"></a><span class="hl line">  749 </span>  nil<span class="hl sym">)</span>
<a id="l_750"></a><span class="hl line">  750 </span>
<a id="l_751"></a><span class="hl line">  751 </span><span class="hl sym">(</span>defmethod explain <span class="hl sym">((</span>operation operation<span class="hl sym">) (</span>component component<span class="hl sym">))</span>
<a id="l_752"></a><span class="hl line">  752 </span>  <span class="hl sym">(</span>format <span class="hl sym">*</span>verbose-out<span class="hl sym">*</span> <span class="hl str">&quot;~&amp;;;; ~A on ~A~%&quot;</span> operation component<span class="hl sym">))</span>
<a id="l_753"></a><span class="hl line">  753 </span>
<a id="l_754"></a><span class="hl line">  754 </span><span class="hl slc">;;; compile-op</span>
<a id="l_755"></a><span class="hl line">  755 </span>
<a id="l_756"></a><span class="hl line">  756 </span><span class="hl sym">(</span>defclass compile-op <span class="hl sym">(</span>operation<span class="hl sym">)</span>
<a id="l_757"></a><span class="hl line">  757 </span>  <span class="hl sym">((</span>proclamations <span class="hl sym">:</span>initarg <span class="hl sym">:</span>proclamations <span class="hl sym">:</span>accessor compile-op-proclamations <span class="hl sym">:</span>initform nil<span class="hl sym">)</span>
<a id="l_758"></a><span class="hl line">  758 </span>   <span class="hl sym">(</span>on-warnings <span class="hl sym">:</span>initarg <span class="hl sym">:</span>on-warnings <span class="hl sym">:</span>accessor operation-on-warnings
<a id="l_759"></a><span class="hl line">  759 </span>                <span class="hl sym">:</span>initform <span class="hl sym">*</span>compile-file-warnings-behaviour<span class="hl sym">*)</span>
<a id="l_760"></a><span class="hl line">  760 </span>   <span class="hl sym">(</span>on-failure <span class="hl sym">:</span>initarg <span class="hl sym">:</span>on-failure <span class="hl sym">:</span>accessor operation-on-failure
<a id="l_761"></a><span class="hl line">  761 </span>               <span class="hl sym">:</span>initform <span class="hl sym">*</span>compile-file-failure-behaviour<span class="hl sym">*)))</span>
<a id="l_762"></a><span class="hl line">  762 </span>
<a id="l_763"></a><span class="hl line">  763 </span><span class="hl sym">(</span>defmethod perform <span class="hl sym">:</span>before <span class="hl sym">((</span>operation compile-op<span class="hl sym">) (</span>c source-file<span class="hl sym">))</span>
<a id="l_764"></a><span class="hl line">  764 </span>  <span class="hl sym">(</span>map nil #<span class="hl sym">'</span>ensure-directories-exist <span class="hl sym">(</span>output-files operation c<span class="hl sym">)))</span>
<a id="l_765"></a><span class="hl line">  765 </span>
<a id="l_766"></a><span class="hl line">  766 </span><span class="hl sym">(</span>defmethod perform <span class="hl sym">:</span>after <span class="hl sym">((</span>operation operation<span class="hl sym">) (</span>c component<span class="hl sym">))</span>
<a id="l_767"></a><span class="hl line">  767 </span>  <span class="hl sym">(</span>setf <span class="hl sym">(</span>gethash <span class="hl sym">(</span><span class="hl kwa">type</span>-of operation<span class="hl sym">) (</span>component-operation-times c<span class="hl sym">))</span>
<a id="l_768"></a><span class="hl line">  768 </span>        <span class="hl sym">(</span>get-universal-time<span class="hl sym">))</span>
<a id="l_769"></a><span class="hl line">  769 </span>  <span class="hl sym">(</span><span class="hl kwa">load</span>-preferences c operation<span class="hl sym">))</span>
<a id="l_770"></a><span class="hl line">  770 </span>
<a id="l_771"></a><span class="hl line">  771 </span><span class="hl slc">;;; perform is required to check output-files to find out where to put</span>
<a id="l_772"></a><span class="hl line">  772 </span><span class="hl slc">;;; its answers, in case it has been overridden for site policy</span>
<a id="l_773"></a><span class="hl line">  773 </span><span class="hl sym">(</span>defmethod perform <span class="hl sym">((</span>operation compile-op<span class="hl sym">) (</span>c cl-source-file<span class="hl sym">))</span>
<a id="l_774"></a><span class="hl line">  774 </span>  #-<span class="hl sym">:</span>broken-fasl-loader
<a id="l_775"></a><span class="hl line">  775 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>source-file <span class="hl sym">(</span>component-pathname c<span class="hl sym">))</span>
<a id="l_776"></a><span class="hl line">  776 </span>        <span class="hl sym">(</span>output-file <span class="hl sym">(</span><span class="hl kwa">car</span> <span class="hl sym">(</span>output-files operation c<span class="hl sym">))))</span>
<a id="l_777"></a><span class="hl line">  777 </span>    <span class="hl sym">(</span>multiple-value-bind <span class="hl sym">(</span>output warnings-p failure-p<span class="hl sym">)</span>
<a id="l_778"></a><span class="hl line">  778 </span>        <span class="hl sym">(</span>compile-file source-file <span class="hl sym">:</span>output-file output-file<span class="hl sym">)</span>
<a id="l_779"></a><span class="hl line">  779 </span>      <span class="hl sym">(</span>when warnings-p
<a id="l_780"></a><span class="hl line">  780 </span>        <span class="hl sym">(</span>case <span class="hl sym">(</span>operation-on-warnings operation<span class="hl sym">)</span>
<a id="l_781"></a><span class="hl line">  781 </span>          <span class="hl sym">(:</span>warn <span class="hl sym">(</span>warn
<a id="l_782"></a><span class="hl line">  782 </span>                  <span class="hl str">&quot;~&#64;&lt;COMPILE-FILE warned while performing ~A on ~A.~&#64;:&gt;&quot;</span>
<a id="l_783"></a><span class="hl line">  783 </span>                  operation c<span class="hl sym">))</span>
<a id="l_784"></a><span class="hl line">  784 </span>          <span class="hl sym">(:</span>error <span class="hl sym">(</span>error <span class="hl sym">'</span>compile-warned <span class="hl sym">:</span>component c <span class="hl sym">:</span>operation operation<span class="hl sym">))</span>
<a id="l_785"></a><span class="hl line">  785 </span>          <span class="hl sym">(:</span>ignore nil<span class="hl sym">)))</span>
<a id="l_786"></a><span class="hl line">  786 </span>      <span class="hl sym">(</span>when failure-p
<a id="l_787"></a><span class="hl line">  787 </span>        <span class="hl sym">(</span>case <span class="hl sym">(</span>operation-on-failure operation<span class="hl sym">)</span>
<a id="l_788"></a><span class="hl line">  788 </span>          <span class="hl sym">(:</span>warn <span class="hl sym">(</span>warn
<a id="l_789"></a><span class="hl line">  789 </span>                  <span class="hl str">&quot;~&#64;&lt;COMPILE-FILE failed while performing ~A on ~A.~&#64;:&gt;&quot;</span>
<a id="l_790"></a><span class="hl line">  790 </span>                  operation c<span class="hl sym">))</span>
<a id="l_791"></a><span class="hl line">  791 </span>          <span class="hl sym">(:</span>error <span class="hl sym">(</span>error <span class="hl sym">'</span>compile-failed <span class="hl sym">:</span>component c <span class="hl sym">:</span>operation operation<span class="hl sym">))</span>
<a id="l_792"></a><span class="hl line">  792 </span>          <span class="hl sym">(:</span>ignore nil<span class="hl sym">)))</span>
<a id="l_793"></a><span class="hl line">  793 </span>      <span class="hl sym">(</span>unless output
<a id="l_794"></a><span class="hl line">  794 </span>        <span class="hl sym">(</span>error <span class="hl sym">'</span>compile-error <span class="hl sym">:</span>component c <span class="hl sym">:</span>operation operation<span class="hl sym">)))))</span>
<a id="l_795"></a><span class="hl line">  795 </span>
<a id="l_796"></a><span class="hl line">  796 </span><span class="hl sym">(</span>defmethod output-files <span class="hl sym">((</span>operation compile-op<span class="hl sym">) (</span>c cl-source-file<span class="hl sym">))</span>
<a id="l_797"></a><span class="hl line">  797 </span>  #-<span class="hl sym">:</span>broken-fasl-loader <span class="hl sym">(</span><span class="hl kwa">list</span> <span class="hl sym">(</span>compile-file-pathname <span class="hl sym">(</span>component-pathname c<span class="hl sym">)))</span>
<a id="l_798"></a><span class="hl line">  798 </span>  #<span class="hl sym">+:</span>broken-fasl-loader <span class="hl sym">(</span><span class="hl kwa">list</span> <span class="hl sym">(</span>component-pathname c<span class="hl sym">)))</span>
<a id="l_799"></a><span class="hl line">  799 </span>
<a id="l_800"></a><span class="hl line">  800 </span><span class="hl sym">(</span>defmethod perform <span class="hl sym">((</span>operation compile-op<span class="hl sym">) (</span>c static-file<span class="hl sym">))</span>
<a id="l_801"></a><span class="hl line">  801 </span>  nil<span class="hl sym">)</span>
<a id="l_802"></a><span class="hl line">  802 </span>
<a id="l_803"></a><span class="hl line">  803 </span><span class="hl sym">(</span>defmethod output-files <span class="hl sym">((</span>operation compile-op<span class="hl sym">) (</span>c static-file<span class="hl sym">))</span>
<a id="l_804"></a><span class="hl line">  804 </span>  nil<span class="hl sym">)</span>
<a id="l_805"></a><span class="hl line">  805 </span>
<a id="l_806"></a><span class="hl line">  806 </span><span class="hl sym">(</span>defmethod input-files <span class="hl sym">((</span>op compile-op<span class="hl sym">) (</span>c static-file<span class="hl sym">))</span>
<a id="l_807"></a><span class="hl line">  807 </span>  nil<span class="hl sym">)</span>
<a id="l_808"></a><span class="hl line">  808 </span>
<a id="l_809"></a><span class="hl line">  809 </span>
<a id="l_810"></a><span class="hl line">  810 </span><span class="hl slc">;;; load-op</span>
<a id="l_811"></a><span class="hl line">  811 </span>
<a id="l_812"></a><span class="hl line">  812 </span><span class="hl sym">(</span>defclass basic-<span class="hl kwa">load</span>-op <span class="hl sym">(</span>operation<span class="hl sym">) ())</span>
<a id="l_813"></a><span class="hl line">  813 </span>
<a id="l_814"></a><span class="hl line">  814 </span><span class="hl sym">(</span>defclass <span class="hl kwa">load</span>-op <span class="hl sym">(</span>basic-<span class="hl kwa">load</span>-op<span class="hl sym">) ())</span>
<a id="l_815"></a><span class="hl line">  815 </span>
<a id="l_816"></a><span class="hl line">  816 </span><span class="hl sym">(</span>defmethod perform <span class="hl sym">((</span>o <span class="hl kwa">load</span>-op<span class="hl sym">) (</span>c cl-source-file<span class="hl sym">))</span>
<a id="l_817"></a><span class="hl line">  817 </span>  <span class="hl sym">(</span><span class="hl kwa">mapcar</span> #<span class="hl sym">'</span><span class="hl kwa">load</span> <span class="hl sym">(</span>input-files o c<span class="hl sym">)))</span>
<a id="l_818"></a><span class="hl line">  818 </span>
<a id="l_819"></a><span class="hl line">  819 </span><span class="hl sym">(</span>defmethod perform <span class="hl sym">((</span>operation <span class="hl kwa">load</span>-op<span class="hl sym">) (</span>c static-file<span class="hl sym">))</span>
<a id="l_820"></a><span class="hl line">  820 </span>  nil<span class="hl sym">)</span>
<a id="l_821"></a><span class="hl line">  821 </span><span class="hl sym">(</span>defmethod operation-done-p <span class="hl sym">((</span>operation <span class="hl kwa">load</span>-op<span class="hl sym">) (</span>c static-file<span class="hl sym">))</span>
<a id="l_822"></a><span class="hl line">  822 </span>  t<span class="hl sym">)</span>
<a id="l_823"></a><span class="hl line">  823 </span>
<a id="l_824"></a><span class="hl line">  824 </span><span class="hl sym">(</span>defmethod output-files <span class="hl sym">((</span>o operation<span class="hl sym">) (</span>c component<span class="hl sym">))</span>
<a id="l_825"></a><span class="hl line">  825 </span>  nil<span class="hl sym">)</span>
<a id="l_826"></a><span class="hl line">  826 </span>
<a id="l_827"></a><span class="hl line">  827 </span><span class="hl sym">(</span>defmethod component-depends-on <span class="hl sym">((</span>operation <span class="hl kwa">load</span>-op<span class="hl sym">) (</span>c component<span class="hl sym">))</span>
<a id="l_828"></a><span class="hl line">  828 </span>  <span class="hl sym">(</span><span class="hl kwa">cons</span> <span class="hl sym">(</span><span class="hl kwa">list</span> <span class="hl sym">'</span>compile-op <span class="hl sym">(</span>component-name c<span class="hl sym">))</span>
<a id="l_829"></a><span class="hl line">  829 </span>        <span class="hl sym">(</span>call-next-method<span class="hl sym">)))</span>
<a id="l_830"></a><span class="hl line">  830 </span>
<a id="l_831"></a><span class="hl line">  831 </span><span class="hl slc">;;; load-source-op</span>
<a id="l_832"></a><span class="hl line">  832 </span>
<a id="l_833"></a><span class="hl line">  833 </span><span class="hl sym">(</span>defclass <span class="hl kwa">load</span>-source-op <span class="hl sym">(</span>basic-<span class="hl kwa">load</span>-op<span class="hl sym">) ())</span>
<a id="l_834"></a><span class="hl line">  834 </span>
<a id="l_835"></a><span class="hl line">  835 </span><span class="hl sym">(</span>defmethod perform <span class="hl sym">((</span>o <span class="hl kwa">load</span>-source-op<span class="hl sym">) (</span>c cl-source-file<span class="hl sym">))</span>
<a id="l_836"></a><span class="hl line">  836 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>source <span class="hl sym">(</span>component-pathname c<span class="hl sym">)))</span>
<a id="l_837"></a><span class="hl line">  837 </span>    <span class="hl sym">(</span>setf <span class="hl sym">(</span>component-property c <span class="hl sym">'</span><span class="hl kwa">last</span>-loaded-as-source<span class="hl sym">)</span>
<a id="l_838"></a><span class="hl line">  838 </span>          <span class="hl sym">(</span><span class="hl kwa">and</span> <span class="hl sym">(</span><span class="hl kwa">load</span> source<span class="hl sym">)</span>
<a id="l_839"></a><span class="hl line">  839 </span>               <span class="hl sym">(</span>get-universal-time<span class="hl sym">)))))</span>
<a id="l_840"></a><span class="hl line">  840 </span>
<a id="l_841"></a><span class="hl line">  841 </span><span class="hl sym">(</span>defmethod perform <span class="hl sym">((</span>operation <span class="hl kwa">load</span>-source-op<span class="hl sym">) (</span>c static-file<span class="hl sym">))</span>
<a id="l_842"></a><span class="hl line">  842 </span>  nil<span class="hl sym">)</span>
<a id="l_843"></a><span class="hl line">  843 </span>
<a id="l_844"></a><span class="hl line">  844 </span><span class="hl sym">(</span>defmethod output-files <span class="hl sym">((</span>operation <span class="hl kwa">load</span>-source-op<span class="hl sym">) (</span>c component<span class="hl sym">))</span>
<a id="l_845"></a><span class="hl line">  845 </span>  nil<span class="hl sym">)</span>
<a id="l_846"></a><span class="hl line">  846 </span>
<a id="l_847"></a><span class="hl line">  847 </span><span class="hl slc">;;; FIXME: we simply copy load-op's dependencies.  this is Just Not Right.</span>
<a id="l_848"></a><span class="hl line">  848 </span><span class="hl sym">(</span>defmethod component-depends-on <span class="hl sym">((</span>o <span class="hl kwa">load</span>-source-op<span class="hl sym">) (</span>c component<span class="hl sym">))</span>
<a id="l_849"></a><span class="hl line">  849 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>what-would-<span class="hl kwa">load</span>-op-do <span class="hl sym">(</span><span class="hl kwa">cdr</span> <span class="hl sym">(</span><span class="hl kwa">assoc</span> <span class="hl sym">'</span><span class="hl kwa">load</span>-op
<a id="l_850"></a><span class="hl line">  850 </span>                                           <span class="hl sym">(</span>slot-value c <span class="hl sym">'</span>in-order-to<span class="hl sym">)))))</span>
<a id="l_851"></a><span class="hl line">  851 </span>    <span class="hl sym">(</span><span class="hl kwa">mapcar</span> <span class="hl sym">(</span><span class="hl kwa">lambda</span> <span class="hl sym">(</span>dep<span class="hl sym">)</span>
<a id="l_852"></a><span class="hl line">  852 </span>              <span class="hl sym">(</span><span class="hl kwa">if</span> <span class="hl sym">(</span><span class="hl kwa">eq</span> <span class="hl sym">(</span><span class="hl kwa">car</span> dep<span class="hl sym">) '</span><span class="hl kwa">load</span>-op<span class="hl sym">)</span>
<a id="l_853"></a><span class="hl line">  853 </span>                  <span class="hl sym">(</span><span class="hl kwa">cons</span> <span class="hl sym">'</span><span class="hl kwa">load</span>-source-op <span class="hl sym">(</span><span class="hl kwa">cdr</span> dep<span class="hl sym">))</span>
<a id="l_854"></a><span class="hl line">  854 </span>                  dep<span class="hl sym">))</span>
<a id="l_855"></a><span class="hl line">  855 </span>            what-would-<span class="hl kwa">load</span>-op-do<span class="hl sym">)))</span>
<a id="l_856"></a><span class="hl line">  856 </span>
<a id="l_857"></a><span class="hl line">  857 </span><span class="hl sym">(</span>defmethod operation-done-p <span class="hl sym">((</span>o <span class="hl kwa">load</span>-source-op<span class="hl sym">) (</span>c source-file<span class="hl sym">))</span>
<a id="l_858"></a><span class="hl line">  858 </span>  <span class="hl sym">(</span><span class="hl kwa">if</span> <span class="hl sym">(</span><span class="hl kwa">or</span> <span class="hl sym">(</span><span class="hl kwa">not</span> <span class="hl sym">(</span>component-property c <span class="hl sym">'</span><span class="hl kwa">last</span>-loaded-as-source<span class="hl sym">))</span>
<a id="l_859"></a><span class="hl line">  859 </span>          <span class="hl sym">(&gt; (</span>file-write-date <span class="hl sym">(</span>component-pathname c<span class="hl sym">))</span>
<a id="l_860"></a><span class="hl line">  860 </span>             <span class="hl sym">(</span>component-property c <span class="hl sym">'</span><span class="hl kwa">last</span>-loaded-as-source<span class="hl sym">)))</span>
<a id="l_861"></a><span class="hl line">  861 </span>      nil t<span class="hl sym">))</span>
<a id="l_862"></a><span class="hl line">  862 </span>
<a id="l_863"></a><span class="hl line">  863 </span><span class="hl sym">(</span>defclass test-op <span class="hl sym">(</span>operation<span class="hl sym">) ())</span>
<a id="l_864"></a><span class="hl line">  864 </span>
<a id="l_865"></a><span class="hl line">  865 </span><span class="hl sym">(</span>defmethod perform <span class="hl sym">((</span>operation test-op<span class="hl sym">) (</span>c component<span class="hl sym">))</span>
<a id="l_866"></a><span class="hl line">  866 </span>  nil<span class="hl sym">)</span>
<a id="l_867"></a><span class="hl line">  867 </span>
<a id="l_868"></a><span class="hl line">  868 </span><span class="hl sym">(</span>defgeneric <span class="hl kwa">load</span>-preferences <span class="hl sym">(</span>system operation<span class="hl sym">)</span>
<a id="l_869"></a><span class="hl line">  869 </span>  <span class="hl sym">(:</span>documentation
<a id="l_870"></a><span class="hl line">  870 </span>   <span class="hl str">&quot;Called to load system preferences after &lt;perform operation</span>
<a id="l_871"></a><span class="hl line">  871 </span><span class="hl str">system&gt;. Typical uses are to set parameters that don't exist until</span>
<a id="l_872"></a><span class="hl line">  872 </span><span class="hl str">after the system has been loaded.&quot;</span><span class="hl sym">))</span>
<a id="l_873"></a><span class="hl line">  873 </span>
<a id="l_874"></a><span class="hl line">  874 </span><span class="hl sym">(</span>defgeneric preference-file-for-system<span class="hl sym">/</span>operation <span class="hl sym">(</span>system operation<span class="hl sym">)</span>
<a id="l_875"></a><span class="hl line">  875 </span>  <span class="hl sym">(:</span>documentation
<a id="l_876"></a><span class="hl line">  876 </span>   <span class="hl str">&quot;Returns the pathname of the preference file for this system.</span>
<a id="l_877"></a><span class="hl line">  877 </span><span class="hl str">Called by 'load-preferences to determine what file to load.&quot;</span><span class="hl sym">))</span>
<a id="l_878"></a><span class="hl line">  878 </span>
<a id="l_879"></a><span class="hl line">  879 </span><span class="hl sym">(</span>defmethod <span class="hl kwa">load</span>-preferences <span class="hl sym">((</span>s t<span class="hl sym">) (</span>operation t<span class="hl sym">))</span>
<a id="l_880"></a><span class="hl line">  880 </span>  <span class="hl slc">;; do nothing</span>
<a id="l_881"></a><span class="hl line">  881 </span>  <span class="hl sym">(</span>values<span class="hl sym">))</span>
<a id="l_882"></a><span class="hl line">  882 </span>
<a id="l_883"></a><span class="hl line">  883 </span><span class="hl sym">(</span>defmethod <span class="hl kwa">load</span>-preferences <span class="hl sym">((</span>s system<span class="hl sym">) (</span>operation basic-<span class="hl kwa">load</span>-op<span class="hl sym">))</span>
<a id="l_884"></a><span class="hl line">  884 </span>  <span class="hl sym">(</span>let<span class="hl sym">* ((*</span>package<span class="hl sym">* (</span>find-package <span class="hl sym">:</span>common-lisp<span class="hl sym">))</span>
<a id="l_885"></a><span class="hl line">  885 </span>         <span class="hl sym">(</span>file <span class="hl sym">(</span>probe-file <span class="hl sym">(</span>preference-file-for-system<span class="hl sym">/</span>operation s operation<span class="hl sym">))))</span>
<a id="l_886"></a><span class="hl line">  886 </span>    <span class="hl sym">(</span>when file
<a id="l_887"></a><span class="hl line">  887 </span>      <span class="hl sym">(</span>when <span class="hl sym">*</span>verbose-out<span class="hl sym">*</span>
<a id="l_888"></a><span class="hl line">  888 </span>        <span class="hl sym">(</span>format <span class="hl sym">*</span>verbose-out<span class="hl sym">*</span>
<a id="l_889"></a><span class="hl line">  889 </span>                <span class="hl str">&quot;~&amp;~&#64;&lt;; ~&#64;;loading preferences for ~A/~(~A~) from ~A~&#64;:&gt;~%&quot;</span>
<a id="l_890"></a><span class="hl line">  890 </span>                <span class="hl sym">(</span>component-name s<span class="hl sym">)</span>
<a id="l_891"></a><span class="hl line">  891 </span>                <span class="hl sym">(</span><span class="hl kwa">type</span>-of operation<span class="hl sym">)</span> file<span class="hl sym">))</span>
<a id="l_892"></a><span class="hl line">  892 </span>      <span class="hl sym">(</span><span class="hl kwa">load</span> file<span class="hl sym">))))</span>
<a id="l_893"></a><span class="hl line">  893 </span>
<a id="l_894"></a><span class="hl line">  894 </span><span class="hl sym">(</span>defmethod preference-file-for-system<span class="hl sym">/</span>operation <span class="hl sym">((</span>system t<span class="hl sym">) (</span>operation t<span class="hl sym">))</span>
<a id="l_895"></a><span class="hl line">  895 </span>  <span class="hl slc">;; cope with anything other than systems</span>
<a id="l_896"></a><span class="hl line">  896 </span>  <span class="hl sym">(</span>preference-file-for-system<span class="hl sym">/</span>operation <span class="hl sym">(</span>find-system system t<span class="hl sym">)</span> operation<span class="hl sym">))</span>
<a id="l_897"></a><span class="hl line">  897 </span>
<a id="l_898"></a><span class="hl line">  898 </span><span class="hl sym">(</span>defmethod preference-file-for-system<span class="hl sym">/</span>operation <span class="hl sym">((</span>s system<span class="hl sym">) (</span>operation t<span class="hl sym">))</span>
<a id="l_899"></a><span class="hl line">  899 </span>  <span class="hl sym">(</span>let <span class="hl sym">((*</span>default-pathname-defaults<span class="hl sym">*</span>
<a id="l_900"></a><span class="hl line">  900 </span>         <span class="hl sym">(</span>make-pathname <span class="hl sym">:</span>name nil <span class="hl sym">:</span><span class="hl kwa">type</span> nil
<a id="l_901"></a><span class="hl line">  901 </span>                        <span class="hl sym">:</span>defaults <span class="hl sym">*</span>default-pathname-defaults<span class="hl sym">*)))</span>
<a id="l_902"></a><span class="hl line">  902 </span>     <span class="hl sym">(</span>merge-pathnames
<a id="l_903"></a><span class="hl line">  903 </span>      <span class="hl sym">(</span>make-pathname <span class="hl sym">:</span>name <span class="hl sym">(</span>component-name s<span class="hl sym">)</span>
<a id="l_904"></a><span class="hl line">  904 </span>                     <span class="hl sym">:</span><span class="hl kwa">type</span> <span class="hl str">&quot;lisp&quot;</span>
<a id="l_905"></a><span class="hl line">  905 </span>                     <span class="hl sym">:</span>directory <span class="hl sym">'(:</span>relative <span class="hl str">&quot;.asdf&quot;</span><span class="hl sym">))</span>
<a id="l_906"></a><span class="hl line">  906 </span>      <span class="hl sym">(</span>truename <span class="hl sym">(</span>user-homedir-pathname<span class="hl sym">)))))</span>
<a id="l_907"></a><span class="hl line">  907 </span>
<a id="l_908"></a><span class="hl line">  908 </span><span class="hl slc">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</span>
<a id="l_909"></a><span class="hl line">  909 </span><span class="hl slc">;;; invoking operations</span>
<a id="l_910"></a><span class="hl line">  910 </span>
<a id="l_911"></a><span class="hl line">  911 </span><span class="hl sym">(</span>defvar <span class="hl sym">*</span>operate-docstring<span class="hl sym">*</span>
<a id="l_912"></a><span class="hl line">  912 </span>  <span class="hl str">&quot;Operate does three things:</span>
<a id="l_913"></a><span class="hl line">  913 </span><span class="hl str"></span>
<a id="l_914"></a><span class="hl line">  914 </span><span class="hl str">1. It creates an instance of `operation-class` using any keyword parameters</span>
<a id="l_915"></a><span class="hl line">  915 </span><span class="hl str">as initargs.</span>
<a id="l_916"></a><span class="hl line">  916 </span><span class="hl str">2. It finds the  asdf-system specified by `system` (possibly loading</span>
<a id="l_917"></a><span class="hl line">  917 </span><span class="hl str">it from disk).</span>
<a id="l_918"></a><span class="hl line">  918 </span><span class="hl str">3. It then calls `traverse` with the operation and system as arguments</span>
<a id="l_919"></a><span class="hl line">  919 </span><span class="hl str"></span>
<a id="l_920"></a><span class="hl line">  920 </span><span class="hl str">The traverse operation is wrapped in `with-compilation-unit` and error</span>
<a id="l_921"></a><span class="hl line">  921 </span><span class="hl str">handling code. If a `version` argument is supplied, then operate also</span>
<a id="l_922"></a><span class="hl line">  922 </span><span class="hl str">ensures that the system found satisfies it using the `version-satisfies`</span>
<a id="l_923"></a><span class="hl line">  923 </span><span class="hl str">method.&quot;</span><span class="hl sym">)</span>
<a id="l_924"></a><span class="hl line">  924 </span>
<a id="l_925"></a><span class="hl line">  925 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> operate <span class="hl sym">(</span>operation-class system <span class="hl sym">&amp;</span>rest args <span class="hl sym">&amp;</span>key <span class="hl sym">(</span>verbose t<span class="hl sym">)</span> version
<a id="l_926"></a><span class="hl line">  926 </span>                <span class="hl sym">&amp;</span>allow-other-keys<span class="hl sym">)</span>
<a id="l_927"></a><span class="hl line">  927 </span>  <span class="hl sym">(</span>let<span class="hl sym">* ((</span>op <span class="hl sym">(</span><span class="hl kwa">apply</span> #<span class="hl sym">'</span>make-instance operation-class
<a id="l_928"></a><span class="hl line">  928 </span>                    <span class="hl sym">:</span>original-initargs args
<a id="l_929"></a><span class="hl line">  929 </span>                    args<span class="hl sym">))</span>
<a id="l_930"></a><span class="hl line">  930 </span>         <span class="hl sym">(*</span>verbose-out<span class="hl sym">* (</span><span class="hl kwa">if</span> verbose <span class="hl sym">*</span>standard-output<span class="hl sym">* (</span>make-broadcast-stream<span class="hl sym">)))</span>
<a id="l_931"></a><span class="hl line">  931 </span>         <span class="hl sym">(</span>system <span class="hl sym">(</span><span class="hl kwa">if</span> <span class="hl sym">(</span>typep system <span class="hl sym">'</span>component<span class="hl sym">)</span> system <span class="hl sym">(</span>find-system system<span class="hl sym">))))</span>
<a id="l_932"></a><span class="hl line">  932 </span>    <span class="hl sym">(</span>unless <span class="hl sym">(</span>version-satisfies system version<span class="hl sym">)</span>
<a id="l_933"></a><span class="hl line">  933 </span>      <span class="hl sym">(</span>error <span class="hl sym">'</span>missing-component <span class="hl sym">:</span>requires system <span class="hl sym">:</span>version version<span class="hl sym">))</span>
<a id="l_934"></a><span class="hl line">  934 </span>    <span class="hl sym">(</span>let <span class="hl sym">((</span>steps <span class="hl sym">(</span>traverse op system<span class="hl sym">)))</span>
<a id="l_935"></a><span class="hl line">  935 </span>      <span class="hl sym">(</span>with-compilation-unit <span class="hl sym">()</span>
<a id="l_936"></a><span class="hl line">  936 </span>        <span class="hl sym">(</span>loop for <span class="hl sym">(</span>op . component<span class="hl sym">)</span> in steps do
<a id="l_937"></a><span class="hl line">  937 </span>                 <span class="hl sym">(</span>loop
<a id="l_938"></a><span class="hl line">  938 </span>                   <span class="hl sym">(</span>restart-case
<a id="l_939"></a><span class="hl line">  939 </span>                       <span class="hl sym">(</span><span class="hl kwa">progn</span> <span class="hl sym">(</span>perform op component<span class="hl sym">)</span>
<a id="l_940"></a><span class="hl line">  940 </span>                              <span class="hl sym">(</span>return<span class="hl sym">))</span>
<a id="l_941"></a><span class="hl line">  941 </span>                     <span class="hl sym">(</span>retry <span class="hl sym">()</span>
<a id="l_942"></a><span class="hl line">  942 </span>                       <span class="hl sym">:</span>report
<a id="l_943"></a><span class="hl line">  943 </span>                       <span class="hl sym">(</span><span class="hl kwa">lambda</span> <span class="hl sym">(</span>s<span class="hl sym">)</span>
<a id="l_944"></a><span class="hl line">  944 </span>                         <span class="hl sym">(</span>format s <span class="hl str">&quot;~&#64;&lt;Retry performing ~S on ~S.~&#64;:&gt;&quot;</span>
<a id="l_945"></a><span class="hl line">  945 </span>                                 op component<span class="hl sym">)))</span>
<a id="l_946"></a><span class="hl line">  946 </span>                     <span class="hl sym">(</span>accept <span class="hl sym">()</span>
<a id="l_947"></a><span class="hl line">  947 </span>                       <span class="hl sym">:</span>report
<a id="l_948"></a><span class="hl line">  948 </span>                       <span class="hl sym">(</span><span class="hl kwa">lambda</span> <span class="hl sym">(</span>s<span class="hl sym">)</span>
<a id="l_949"></a><span class="hl line">  949 </span>                         <span class="hl sym">(</span>format s <span class="hl str">&quot;~&#64;&lt;Continue, treating ~S on ~S as ~</span>
<a id="l_950"></a><span class="hl line">  950 </span><span class="hl str">                                   having been successful.~&#64;:&gt;&quot;</span>
<a id="l_951"></a><span class="hl line">  951 </span>                                 op component<span class="hl sym">))</span>
<a id="l_952"></a><span class="hl line">  952 </span>                       <span class="hl sym">(</span>setf <span class="hl sym">(</span>gethash <span class="hl sym">(</span><span class="hl kwa">type</span>-of op<span class="hl sym">)</span>
<a id="l_953"></a><span class="hl line">  953 </span>                                      <span class="hl sym">(</span>component-operation-times component<span class="hl sym">))</span>
<a id="l_954"></a><span class="hl line">  954 </span>                             <span class="hl sym">(</span>get-universal-time<span class="hl sym">))</span>
<a id="l_955"></a><span class="hl line">  955 </span>                       <span class="hl sym">(</span>return<span class="hl sym">)))))))))</span>
<a id="l_956"></a><span class="hl line">  956 </span>
<a id="l_957"></a><span class="hl line">  957 </span><span class="hl sym">(</span>setf <span class="hl sym">(</span>documentation <span class="hl sym">'</span>operate <span class="hl sym">'</span>function<span class="hl sym">)</span>
<a id="l_958"></a><span class="hl line">  958 </span>      <span class="hl sym">*</span>operate-docstring<span class="hl sym">*)</span>
<a id="l_959"></a><span class="hl line">  959 </span>
<a id="l_960"></a><span class="hl line">  960 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> oos <span class="hl sym">(</span>operation-class system <span class="hl sym">&amp;</span>rest args <span class="hl sym">&amp;</span>key force <span class="hl sym">(</span>verbose t<span class="hl sym">)</span> version<span class="hl sym">)</span>
<a id="l_961"></a><span class="hl line">  961 </span>  <span class="hl sym">(</span>declare <span class="hl sym">(</span>ignore force verbose version<span class="hl sym">))</span>
<a id="l_962"></a><span class="hl line">  962 </span>  <span class="hl sym">(</span><span class="hl kwa">apply</span> #<span class="hl sym">'</span>operate operation-class system args<span class="hl sym">))</span>
<a id="l_963"></a><span class="hl line">  963 </span>
<a id="l_964"></a><span class="hl line">  964 </span><span class="hl sym">(</span>setf <span class="hl sym">(</span>documentation <span class="hl sym">'</span>oos <span class="hl sym">'</span>function<span class="hl sym">)</span>
<a id="l_965"></a><span class="hl line">  965 </span>      <span class="hl sym">(</span>format nil
<a id="l_966"></a><span class="hl line">  966 </span>              <span class="hl str">&quot;Short for _operate on system_ and an alias for the `operate` function. ~&amp;~&amp;~a&quot;</span>
<a id="l_967"></a><span class="hl line">  967 </span>              <span class="hl sym">*</span>operate-docstring<span class="hl sym">*))</span>
<a id="l_968"></a><span class="hl line">  968 </span>
<a id="l_969"></a><span class="hl line">  969 </span><span class="hl slc">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</span>
<a id="l_970"></a><span class="hl line">  970 </span><span class="hl slc">;;; syntax</span>
<a id="l_971"></a><span class="hl line">  971 </span>
<a id="l_972"></a><span class="hl line">  972 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> remove-keyword <span class="hl sym">(</span>key arglist<span class="hl sym">)</span>
<a id="l_973"></a><span class="hl line">  973 </span>  <span class="hl sym">(</span>labels <span class="hl sym">((</span>aux <span class="hl sym">(</span>key arglist<span class="hl sym">)</span>
<a id="l_974"></a><span class="hl line">  974 </span>             <span class="hl sym">(</span><span class="hl kwa">cond</span> <span class="hl sym">((</span><span class="hl kwa">null</span> arglist<span class="hl sym">)</span> nil<span class="hl sym">)</span>
<a id="l_975"></a><span class="hl line">  975 </span>                   <span class="hl sym">((</span><span class="hl kwa">eq</span> key <span class="hl sym">(</span><span class="hl kwa">car</span> arglist<span class="hl sym">)) (</span><span class="hl kwa">cddr</span> arglist<span class="hl sym">))</span>
<a id="l_976"></a><span class="hl line">  976 </span>                   <span class="hl sym">(</span>t <span class="hl sym">(</span><span class="hl kwa">cons</span> <span class="hl sym">(</span><span class="hl kwa">car</span> arglist<span class="hl sym">) (</span><span class="hl kwa">cons</span> <span class="hl sym">(</span><span class="hl kwa">cadr</span> arglist<span class="hl sym">)</span>
<a id="l_977"></a><span class="hl line">  977 </span>                                                <span class="hl sym">(</span>remove-keyword
<a id="l_978"></a><span class="hl line">  978 </span>                                                 key <span class="hl sym">(</span><span class="hl kwa">cddr</span> arglist<span class="hl sym">))))))))</span>
<a id="l_979"></a><span class="hl line">  979 </span>    <span class="hl sym">(</span>aux key arglist<span class="hl sym">)))</span>
<a id="l_980"></a><span class="hl line">  980 </span>
<a id="l_981"></a><span class="hl line">  981 </span><span class="hl sym">(</span>defmacro defsystem <span class="hl sym">(</span>name <span class="hl sym">&amp;</span>body options<span class="hl sym">)</span>
<a id="l_982"></a><span class="hl line">  982 </span>  <span class="hl sym">(</span>destructuring-bind <span class="hl sym">(&amp;</span>key <span class="hl sym">(</span>pathname nil pathname-arg-p<span class="hl sym">) (</span>class <span class="hl sym">'</span>system<span class="hl sym">)</span>
<a id="l_983"></a><span class="hl line">  983 </span>                            <span class="hl sym">&amp;</span>allow-other-keys<span class="hl sym">)</span>
<a id="l_984"></a><span class="hl line">  984 </span>      options
<a id="l_985"></a><span class="hl line">  985 </span>    <span class="hl sym">(</span>let <span class="hl sym">((</span>component-options <span class="hl sym">(</span>remove-keyword <span class="hl sym">:</span>class options<span class="hl sym">)))</span>
<a id="l_986"></a><span class="hl line">  986 </span>      `<span class="hl sym">(</span><span class="hl kwa">progn</span>
<a id="l_987"></a><span class="hl line">  987 </span>         <span class="hl slc">;; system must be registered before we parse the body, otherwise</span>
<a id="l_988"></a><span class="hl line">  988 </span>         <span class="hl slc">;; we recur when trying to find an existing system of the same name</span>
<a id="l_989"></a><span class="hl line">  989 </span>         <span class="hl slc">;; to reuse options (e.g. pathname) from</span>
<a id="l_990"></a><span class="hl line">  990 </span>         <span class="hl sym">(</span>let <span class="hl sym">((</span>s <span class="hl sym">(</span>system-registered-p <span class="hl sym">',</span>name<span class="hl sym">)))</span>
<a id="l_991"></a><span class="hl line">  991 </span>           <span class="hl sym">(</span><span class="hl kwa">cond</span> <span class="hl sym">((</span><span class="hl kwa">and</span> s <span class="hl sym">(</span><span class="hl kwa">eq</span> <span class="hl sym">(</span><span class="hl kwa">type</span>-of <span class="hl sym">(</span><span class="hl kwa">cdr</span> s<span class="hl sym">)) ',</span>class<span class="hl sym">))</span>
<a id="l_992"></a><span class="hl line">  992 </span>                  <span class="hl sym">(</span>setf <span class="hl sym">(</span><span class="hl kwa">car</span> s<span class="hl sym">) (</span>get-universal-time<span class="hl sym">)))</span>
<a id="l_993"></a><span class="hl line">  993 </span>                 <span class="hl sym">(</span>s
<a id="l_994"></a><span class="hl line">  994 </span>                  #<span class="hl sym">+</span>clisp
<a id="l_995"></a><span class="hl line">  995 </span>                  <span class="hl sym">(</span>sysdef-error <span class="hl str">&quot;Cannot redefine the existing system ~A with a different class&quot;</span> s<span class="hl sym">)</span>
<a id="l_996"></a><span class="hl line">  996 </span>                  #-clisp
<a id="l_997"></a><span class="hl line">  997 </span>                  <span class="hl sym">(</span>change-class <span class="hl sym">(</span><span class="hl kwa">cdr</span> s<span class="hl sym">) ',</span>class<span class="hl sym">))</span>
<a id="l_998"></a><span class="hl line">  998 </span>                 <span class="hl sym">(</span>t
<a id="l_999"></a><span class="hl line">  999 </span>                  <span class="hl sym">(</span>register-system <span class="hl sym">(</span><span class="hl kwa">quote</span> <span class="hl sym">,</span>name<span class="hl sym">)</span>
<a id="l_1000"></a><span class="hl line"> 1000 </span>                                   <span class="hl sym">(</span>make-instance <span class="hl sym">',</span>class <span class="hl sym">:</span>name <span class="hl sym">',</span>name<span class="hl sym">)))))</span>
<a id="l_1001"></a><span class="hl line"> 1001 </span>         <span class="hl sym">(</span>parse-component-form nil <span class="hl sym">(</span><span class="hl kwa">apply</span>
<a id="l_1002"></a><span class="hl line"> 1002 </span>                                    #<span class="hl sym">'</span><span class="hl kwa">list</span>
<a id="l_1003"></a><span class="hl line"> 1003 </span>                                    <span class="hl sym">:</span>module <span class="hl sym">(</span>coerce-name <span class="hl sym">',</span>name<span class="hl sym">)</span>
<a id="l_1004"></a><span class="hl line"> 1004 </span>                                    <span class="hl sym">:</span>pathname
<a id="l_1005"></a><span class="hl line"> 1005 </span>                                    <span class="hl slc">;; to avoid a note about unreachable code</span>
<a id="l_1006"></a><span class="hl line"> 1006 </span>                                    <span class="hl sym">,(</span><span class="hl kwa">if</span> pathname-arg-p
<a id="l_1007"></a><span class="hl line"> 1007 </span>                                         pathname
<a id="l_1008"></a><span class="hl line"> 1008 </span>                                         `<span class="hl sym">(</span><span class="hl kwa">or</span> <span class="hl sym">(</span>when <span class="hl sym">*</span><span class="hl kwa">load</span>-truename<span class="hl sym">*</span>
<a id="l_1009"></a><span class="hl line"> 1009 </span>                                                <span class="hl sym">(</span>pathname-sans-name<span class="hl sym">+</span><span class="hl kwa">type</span>
<a id="l_1010"></a><span class="hl line"> 1010 </span>                                                 <span class="hl sym">(</span>resolve-symlinks
<a id="l_1011"></a><span class="hl line"> 1011 </span>                                                  <span class="hl sym">*</span><span class="hl kwa">load</span>-truename<span class="hl sym">*)))</span>
<a id="l_1012"></a><span class="hl line"> 1012 </span>                                              <span class="hl sym">*</span>default-pathname-defaults<span class="hl sym">*))</span>
<a id="l_1013"></a><span class="hl line"> 1013 </span>                                    <span class="hl sym">',</span>component-options<span class="hl sym">))))))</span>
<a id="l_1014"></a><span class="hl line"> 1014 </span>
<a id="l_1015"></a><span class="hl line"> 1015 </span>
<a id="l_1016"></a><span class="hl line"> 1016 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> class-for-<span class="hl kwa">type</span> <span class="hl sym">(</span>parent <span class="hl kwa">type</span><span class="hl sym">)</span>
<a id="l_1017"></a><span class="hl line"> 1017 </span>  <span class="hl sym">(</span>let<span class="hl sym">* ((</span>extra-symbols <span class="hl sym">(</span><span class="hl kwa">list</span> <span class="hl sym">(</span>find-symbol <span class="hl sym">(</span>symbol-name <span class="hl kwa">type</span><span class="hl sym">) *</span>package<span class="hl sym">*)</span>
<a id="l_1018"></a><span class="hl line"> 1018 </span>                              <span class="hl sym">(</span>find-symbol <span class="hl sym">(</span>symbol-name <span class="hl kwa">type</span><span class="hl sym">)</span>
<a id="l_1019"></a><span class="hl line"> 1019 </span>                                           <span class="hl sym">(</span><span class="hl kwa">load</span>-time-value
<a id="l_1020"></a><span class="hl line"> 1020 </span>                                            <span class="hl sym">(</span>package-name <span class="hl sym">:</span>asdf<span class="hl sym">)))))</span>
<a id="l_1021"></a><span class="hl line"> 1021 </span>         <span class="hl sym">(</span>class <span class="hl sym">(</span>dolist <span class="hl sym">(</span>symbol <span class="hl sym">(</span><span class="hl kwa">if</span> <span class="hl sym">(</span>keywordp <span class="hl kwa">type</span><span class="hl sym">)</span>
<a id="l_1022"></a><span class="hl line"> 1022 </span>                                    extra-symbols
<a id="l_1023"></a><span class="hl line"> 1023 </span>                                    <span class="hl sym">(</span><span class="hl kwa">cons type</span> extra-symbols<span class="hl sym">)))</span>
<a id="l_1024"></a><span class="hl line"> 1024 </span>                  <span class="hl sym">(</span>when <span class="hl sym">(</span><span class="hl kwa">and</span> symbol
<a id="l_1025"></a><span class="hl line"> 1025 </span>                             <span class="hl sym">(</span>find-class symbol nil<span class="hl sym">)</span>
<a id="l_1026"></a><span class="hl line"> 1026 </span>                             <span class="hl sym">(</span>subtypep symbol <span class="hl sym">'</span>component<span class="hl sym">))</span>
<a id="l_1027"></a><span class="hl line"> 1027 </span>                    <span class="hl sym">(</span>return <span class="hl sym">(</span>find-class symbol<span class="hl sym">))))))</span>
<a id="l_1028"></a><span class="hl line"> 1028 </span>    <span class="hl sym">(</span><span class="hl kwa">or</span> class
<a id="l_1029"></a><span class="hl line"> 1029 </span>        <span class="hl sym">(</span><span class="hl kwa">and</span> <span class="hl sym">(</span><span class="hl kwa">eq type</span> <span class="hl sym">:</span>file<span class="hl sym">)</span>
<a id="l_1030"></a><span class="hl line"> 1030 </span>             <span class="hl sym">(</span><span class="hl kwa">or</span> <span class="hl sym">(</span>module-default-component-class parent<span class="hl sym">)</span>
<a id="l_1031"></a><span class="hl line"> 1031 </span>                 <span class="hl sym">(</span>find-class <span class="hl sym">'</span>cl-source-file<span class="hl sym">)))</span>
<a id="l_1032"></a><span class="hl line"> 1032 </span>        <span class="hl sym">(</span>sysdef-error <span class="hl str">&quot;~&#64;&lt;don't recognize component type ~A~&#64;:&gt;&quot;</span> <span class="hl kwa">type</span><span class="hl sym">))))</span>
<a id="l_1033"></a><span class="hl line"> 1033 </span>
<a id="l_1034"></a><span class="hl line"> 1034 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> maybe-add-tree <span class="hl sym">(</span>tree op1 op2 c<span class="hl sym">)</span>
<a id="l_1035"></a><span class="hl line"> 1035 </span>  <span class="hl str">&quot;Add the node C at /OP1/OP2 in TREE, unless it's there already.</span>
<a id="l_1036"></a><span class="hl line"> 1036 </span><span class="hl str">Returns the new tree (which probably shares structure with the old one)&quot;</span>
<a id="l_1037"></a><span class="hl line"> 1037 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>first-op-tree <span class="hl sym">(</span><span class="hl kwa">assoc</span> op1 tree<span class="hl sym">)))</span>
<a id="l_1038"></a><span class="hl line"> 1038 </span>    <span class="hl sym">(</span><span class="hl kwa">if</span> first-op-tree
<a id="l_1039"></a><span class="hl line"> 1039 </span>        <span class="hl sym">(</span><span class="hl kwa">progn</span>
<a id="l_1040"></a><span class="hl line"> 1040 </span>          <span class="hl sym">(</span>aif <span class="hl sym">(</span><span class="hl kwa">assoc</span> op2 <span class="hl sym">(</span><span class="hl kwa">cdr</span> first-op-tree<span class="hl sym">))</span>
<a id="l_1041"></a><span class="hl line"> 1041 </span>               <span class="hl sym">(</span><span class="hl kwa">if</span> <span class="hl sym">(</span>find c <span class="hl sym">(</span><span class="hl kwa">cdr</span> it<span class="hl sym">))</span>
<a id="l_1042"></a><span class="hl line"> 1042 </span>                   nil
<a id="l_1043"></a><span class="hl line"> 1043 </span>                   <span class="hl sym">(</span>setf <span class="hl sym">(</span><span class="hl kwa">cdr</span> it<span class="hl sym">) (</span><span class="hl kwa">cons</span> c <span class="hl sym">(</span><span class="hl kwa">cdr</span> it<span class="hl sym">))))</span>
<a id="l_1044"></a><span class="hl line"> 1044 </span>               <span class="hl sym">(</span>setf <span class="hl sym">(</span><span class="hl kwa">cdr</span> first-op-tree<span class="hl sym">)</span>
<a id="l_1045"></a><span class="hl line"> 1045 </span>                     <span class="hl sym">(</span>acons op2 <span class="hl sym">(</span><span class="hl kwa">list</span> c<span class="hl sym">) (</span><span class="hl kwa">cdr</span> first-op-tree<span class="hl sym">))))</span>
<a id="l_1046"></a><span class="hl line"> 1046 </span>          tree<span class="hl sym">)</span>
<a id="l_1047"></a><span class="hl line"> 1047 </span>        <span class="hl sym">(</span>acons op1 <span class="hl sym">(</span><span class="hl kwa">list</span> <span class="hl sym">(</span><span class="hl kwa">list</span> op2 c<span class="hl sym">))</span> tree<span class="hl sym">))))</span>
<a id="l_1048"></a><span class="hl line"> 1048 </span>
<a id="l_1049"></a><span class="hl line"> 1049 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> union-of-dependencies <span class="hl sym">(&amp;</span>rest deps<span class="hl sym">)</span>
<a id="l_1050"></a><span class="hl line"> 1050 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>new-tree nil<span class="hl sym">))</span>
<a id="l_1051"></a><span class="hl line"> 1051 </span>    <span class="hl sym">(</span>dolist <span class="hl sym">(</span>dep deps<span class="hl sym">)</span>
<a id="l_1052"></a><span class="hl line"> 1052 </span>      <span class="hl sym">(</span>dolist <span class="hl sym">(</span>op-tree dep<span class="hl sym">)</span>
<a id="l_1053"></a><span class="hl line"> 1053 </span>        <span class="hl sym">(</span>dolist <span class="hl sym">(</span>op  <span class="hl sym">(</span><span class="hl kwa">cdr</span> op-tree<span class="hl sym">))</span>
<a id="l_1054"></a><span class="hl line"> 1054 </span>          <span class="hl sym">(</span>dolist <span class="hl sym">(</span>c <span class="hl sym">(</span><span class="hl kwa">cdr</span> op<span class="hl sym">))</span>
<a id="l_1055"></a><span class="hl line"> 1055 </span>            <span class="hl sym">(</span>setf new-tree
<a id="l_1056"></a><span class="hl line"> 1056 </span>                  <span class="hl sym">(</span>maybe-add-tree new-tree <span class="hl sym">(</span><span class="hl kwa">car</span> op-tree<span class="hl sym">) (</span><span class="hl kwa">car</span> op<span class="hl sym">)</span> c<span class="hl sym">))))))</span>
<a id="l_1057"></a><span class="hl line"> 1057 </span>    new-tree<span class="hl sym">))</span>
<a id="l_1058"></a><span class="hl line"> 1058 </span>
<a id="l_1059"></a><span class="hl line"> 1059 </span>
<a id="l_1060"></a><span class="hl line"> 1060 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> remove-keys <span class="hl sym">(</span>key-names args<span class="hl sym">)</span>
<a id="l_1061"></a><span class="hl line"> 1061 </span>  <span class="hl sym">(</span>loop for <span class="hl sym">(</span> name val <span class="hl sym">)</span> on args by #<span class="hl sym">'</span><span class="hl kwa">cddr</span>
<a id="l_1062"></a><span class="hl line"> 1062 </span>        unless <span class="hl sym">(</span><span class="hl kwa">member</span> <span class="hl sym">(</span>symbol-name name<span class="hl sym">)</span> key-names
<a id="l_1063"></a><span class="hl line"> 1063 </span>                       <span class="hl sym">:</span>key #<span class="hl sym">'</span>symbol-name <span class="hl sym">:</span>test <span class="hl sym">'</span><span class="hl kwa">equal</span><span class="hl sym">)</span>
<a id="l_1064"></a><span class="hl line"> 1064 </span>        <span class="hl kwa">append</span> <span class="hl sym">(</span><span class="hl kwa">list</span> name val<span class="hl sym">)))</span>
<a id="l_1065"></a><span class="hl line"> 1065 </span>
<a id="l_1066"></a><span class="hl line"> 1066 </span><span class="hl sym">(</span>defvar <span class="hl sym">*</span>serial-depends-on<span class="hl sym">*)</span>
<a id="l_1067"></a><span class="hl line"> 1067 </span>
<a id="l_1068"></a><span class="hl line"> 1068 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> parse-component-form <span class="hl sym">(</span>parent options<span class="hl sym">)</span>
<a id="l_1069"></a><span class="hl line"> 1069 </span>
<a id="l_1070"></a><span class="hl line"> 1070 </span>  <span class="hl sym">(</span>destructuring-bind
<a id="l_1071"></a><span class="hl line"> 1071 </span>        <span class="hl sym">(</span><span class="hl kwa">type</span> name <span class="hl sym">&amp;</span>rest rest <span class="hl sym">&amp;</span>key
<a id="l_1072"></a><span class="hl line"> 1072 </span>              <span class="hl slc">;; the following list of keywords is reproduced below in the</span>
<a id="l_1073"></a><span class="hl line"> 1073 </span>              <span class="hl slc">;; remove-keys form.  important to keep them in sync</span>
<a id="l_1074"></a><span class="hl line"> 1074 </span>              components pathname default-component-class
<a id="l_1075"></a><span class="hl line"> 1075 </span>              perform explain output-files operation-done-p
<a id="l_1076"></a><span class="hl line"> 1076 </span>              weakly-depends-on
<a id="l_1077"></a><span class="hl line"> 1077 </span>              depends-on serial in-order-to
<a id="l_1078"></a><span class="hl line"> 1078 </span>              <span class="hl slc">;; list ends</span>
<a id="l_1079"></a><span class="hl line"> 1079 </span>              <span class="hl sym">&amp;</span>allow-other-keys<span class="hl sym">)</span> options
<a id="l_1080"></a><span class="hl line"> 1080 </span>    <span class="hl sym">(</span>declare <span class="hl sym">(</span>ignorable perform explain output-files operation-done-p<span class="hl sym">))</span>
<a id="l_1081"></a><span class="hl line"> 1081 </span>    <span class="hl sym">(</span>check-component-input <span class="hl kwa">type</span> name weakly-depends-on depends-on components in-order-to<span class="hl sym">)</span>
<a id="l_1082"></a><span class="hl line"> 1082 </span>
<a id="l_1083"></a><span class="hl line"> 1083 </span>    <span class="hl sym">(</span>when <span class="hl sym">(</span><span class="hl kwa">and</span> parent
<a id="l_1084"></a><span class="hl line"> 1084 </span>               <span class="hl sym">(</span>find-component parent name<span class="hl sym">)</span>
<a id="l_1085"></a><span class="hl line"> 1085 </span>               <span class="hl slc">;; ignore the same object when rereading the defsystem</span>
<a id="l_1086"></a><span class="hl line"> 1086 </span>               <span class="hl sym">(</span><span class="hl kwa">not</span>
<a id="l_1087"></a><span class="hl line"> 1087 </span>                <span class="hl sym">(</span>typep <span class="hl sym">(</span>find-component parent name<span class="hl sym">)</span>
<a id="l_1088"></a><span class="hl line"> 1088 </span>                       <span class="hl sym">(</span>class-for-<span class="hl kwa">type</span> parent <span class="hl kwa">type</span><span class="hl sym">))))</span>
<a id="l_1089"></a><span class="hl line"> 1089 </span>      <span class="hl sym">(</span>error <span class="hl sym">'</span>duplicate-names <span class="hl sym">:</span>name name<span class="hl sym">))</span>
<a id="l_1090"></a><span class="hl line"> 1090 </span>
<a id="l_1091"></a><span class="hl line"> 1091 </span>    <span class="hl sym">(</span>let<span class="hl sym">* ((</span>other-args <span class="hl sym">(</span>remove-keys
<a id="l_1092"></a><span class="hl line"> 1092 </span>                        <span class="hl sym">'(</span>components pathname default-component-class
<a id="l_1093"></a><span class="hl line"> 1093 </span>                          perform explain output-files operation-done-p
<a id="l_1094"></a><span class="hl line"> 1094 </span>                          weakly-depends-on
<a id="l_1095"></a><span class="hl line"> 1095 </span>                          depends-on serial in-order-to<span class="hl sym">)</span>
<a id="l_1096"></a><span class="hl line"> 1096 </span>                        rest<span class="hl sym">))</span>
<a id="l_1097"></a><span class="hl line"> 1097 </span>           <span class="hl sym">(</span>ret
<a id="l_1098"></a><span class="hl line"> 1098 </span>            <span class="hl sym">(</span><span class="hl kwa">or</span> <span class="hl sym">(</span>find-component parent name<span class="hl sym">)</span>
<a id="l_1099"></a><span class="hl line"> 1099 </span>                <span class="hl sym">(</span>make-instance <span class="hl sym">(</span>class-for-<span class="hl kwa">type</span> parent <span class="hl kwa">type</span><span class="hl sym">)))))</span>
<a id="l_1100"></a><span class="hl line"> 1100 </span>      <span class="hl sym">(</span>when weakly-depends-on
<a id="l_1101"></a><span class="hl line"> 1101 </span>        <span class="hl sym">(</span>setf depends-on <span class="hl sym">(</span><span class="hl kwa">append</span> depends-on <span class="hl sym">(</span>remove-<span class="hl kwa">if</span> <span class="hl sym">(</span>complement #<span class="hl sym">'</span>find-system<span class="hl sym">)</span> weakly-depends-on<span class="hl sym">))))</span>
<a id="l_1102"></a><span class="hl line"> 1102 </span>      <span class="hl sym">(</span>when <span class="hl sym">(</span><span class="hl kwa">boundp</span> <span class="hl sym">'*</span>serial-depends-on<span class="hl sym">*)</span>
<a id="l_1103"></a><span class="hl line"> 1103 </span>        <span class="hl sym">(</span>setf depends-on
<a id="l_1104"></a><span class="hl line"> 1104 </span>              <span class="hl sym">(</span>concatenate <span class="hl sym">'</span><span class="hl kwa">list</span> <span class="hl sym">*</span>serial-depends-on<span class="hl sym">*</span> depends-on<span class="hl sym">)))</span>
<a id="l_1105"></a><span class="hl line"> 1105 </span>      <span class="hl sym">(</span><span class="hl kwa">apply</span> #<span class="hl sym">'</span>reinitialize-instance ret
<a id="l_1106"></a><span class="hl line"> 1106 </span>             <span class="hl sym">:</span>name <span class="hl sym">(</span>coerce-name name<span class="hl sym">)</span>
<a id="l_1107"></a><span class="hl line"> 1107 </span>             <span class="hl sym">:</span>pathname pathname
<a id="l_1108"></a><span class="hl line"> 1108 </span>             <span class="hl sym">:</span>parent parent
<a id="l_1109"></a><span class="hl line"> 1109 </span>             other-args<span class="hl sym">)</span>
<a id="l_1110"></a><span class="hl line"> 1110 </span>      <span class="hl sym">(</span>when <span class="hl sym">(</span>typep ret <span class="hl sym">'</span>module<span class="hl sym">)</span>
<a id="l_1111"></a><span class="hl line"> 1111 </span>        <span class="hl sym">(</span>setf <span class="hl sym">(</span>module-default-component-class ret<span class="hl sym">)</span>
<a id="l_1112"></a><span class="hl line"> 1112 </span>              <span class="hl sym">(</span><span class="hl kwa">or</span> default-component-class
<a id="l_1113"></a><span class="hl line"> 1113 </span>                  <span class="hl sym">(</span><span class="hl kwa">and</span> <span class="hl sym">(</span>typep parent <span class="hl sym">'</span>module<span class="hl sym">)</span>
<a id="l_1114"></a><span class="hl line"> 1114 </span>                       <span class="hl sym">(</span>module-default-component-class parent<span class="hl sym">))))</span>
<a id="l_1115"></a><span class="hl line"> 1115 </span>        <span class="hl sym">(</span>let <span class="hl sym">((*</span>serial-depends-on<span class="hl sym">*</span> nil<span class="hl sym">))</span>
<a id="l_1116"></a><span class="hl line"> 1116 </span>          <span class="hl sym">(</span>setf <span class="hl sym">(</span>module-components ret<span class="hl sym">)</span>
<a id="l_1117"></a><span class="hl line"> 1117 </span>                <span class="hl sym">(</span>loop for c-form in components
<a id="l_1118"></a><span class="hl line"> 1118 </span>                      for c <span class="hl sym">= (</span>parse-component-form ret c-form<span class="hl sym">)</span>
<a id="l_1119"></a><span class="hl line"> 1119 </span>                      collect c
<a id="l_1120"></a><span class="hl line"> 1120 </span>                      <span class="hl kwa">if</span> serial
<a id="l_1121"></a><span class="hl line"> 1121 </span>                      do <span class="hl sym">(</span>push <span class="hl sym">(</span>component-name c<span class="hl sym">) *</span>serial-depends-on<span class="hl sym">*))))</span>
<a id="l_1122"></a><span class="hl line"> 1122 </span>
<a id="l_1123"></a><span class="hl line"> 1123 </span>        <span class="hl slc">;; check for duplicate names</span>
<a id="l_1124"></a><span class="hl line"> 1124 </span>        <span class="hl sym">(</span>let <span class="hl sym">((</span>name-hash <span class="hl sym">(</span>make-hash-table <span class="hl sym">:</span>test #<span class="hl sym">'</span><span class="hl kwa">equal</span><span class="hl sym">)))</span>
<a id="l_1125"></a><span class="hl line"> 1125 </span>          <span class="hl sym">(</span>loop for c in <span class="hl sym">(</span>module-components ret<span class="hl sym">)</span>
<a id="l_1126"></a><span class="hl line"> 1126 </span>                do
<a id="l_1127"></a><span class="hl line"> 1127 </span>                <span class="hl sym">(</span><span class="hl kwa">if</span> <span class="hl sym">(</span>gethash <span class="hl sym">(</span>component-name c<span class="hl sym">)</span>
<a id="l_1128"></a><span class="hl line"> 1128 </span>                             name-hash<span class="hl sym">)</span>
<a id="l_1129"></a><span class="hl line"> 1129 </span>                    <span class="hl sym">(</span>error <span class="hl sym">'</span>duplicate-names
<a id="l_1130"></a><span class="hl line"> 1130 </span>                           <span class="hl sym">:</span>name <span class="hl sym">(</span>component-name c<span class="hl sym">))</span>
<a id="l_1131"></a><span class="hl line"> 1131 </span>                    <span class="hl sym">(</span>setf <span class="hl sym">(</span>gethash <span class="hl sym">(</span>component-name c<span class="hl sym">)</span>
<a id="l_1132"></a><span class="hl line"> 1132 </span>                                   name-hash<span class="hl sym">)</span>
<a id="l_1133"></a><span class="hl line"> 1133 </span>                          t<span class="hl sym">)))))</span>
<a id="l_1134"></a><span class="hl line"> 1134 </span>
<a id="l_1135"></a><span class="hl line"> 1135 </span>      <span class="hl sym">(</span>setf <span class="hl sym">(</span>slot-value ret <span class="hl sym">'</span>in-order-to<span class="hl sym">)</span>
<a id="l_1136"></a><span class="hl line"> 1136 </span>            <span class="hl sym">(</span>union-of-dependencies
<a id="l_1137"></a><span class="hl line"> 1137 </span>             in-order-to
<a id="l_1138"></a><span class="hl line"> 1138 </span>             `<span class="hl sym">((</span>compile-op <span class="hl sym">(</span>compile-op <span class="hl sym">,</span>&#64;depends-on<span class="hl sym">))</span>
<a id="l_1139"></a><span class="hl line"> 1139 </span>               <span class="hl sym">(</span><span class="hl kwa">load</span>-op <span class="hl sym">(</span><span class="hl kwa">load</span>-op <span class="hl sym">,</span>&#64;depends-on<span class="hl sym">))))</span>
<a id="l_1140"></a><span class="hl line"> 1140 </span>            <span class="hl sym">(</span>slot-value ret <span class="hl sym">'</span>do-first<span class="hl sym">)</span> `<span class="hl sym">((</span>compile-op <span class="hl sym">(</span><span class="hl kwa">load</span>-op <span class="hl sym">,</span>&#64;depends-on<span class="hl sym">))))</span>
<a id="l_1141"></a><span class="hl line"> 1141 </span>
<a id="l_1142"></a><span class="hl line"> 1142 </span>      <span class="hl sym">(</span>%remove-component-inline-methods ret rest<span class="hl sym">)</span>
<a id="l_1143"></a><span class="hl line"> 1143 </span>
<a id="l_1144"></a><span class="hl line"> 1144 </span>      ret<span class="hl sym">)))</span>
<a id="l_1145"></a><span class="hl line"> 1145 </span>
<a id="l_1146"></a><span class="hl line"> 1146 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> %remove-component-inline-methods <span class="hl sym">(</span>ret rest<span class="hl sym">)</span>
<a id="l_1147"></a><span class="hl line"> 1147 </span>  <span class="hl sym">(</span>loop for name in <span class="hl sym">+</span>asdf-methods<span class="hl sym">+</span>
<a id="l_1148"></a><span class="hl line"> 1148 </span>        do <span class="hl sym">(</span>map <span class="hl sym">'</span>nil
<a id="l_1149"></a><span class="hl line"> 1149 </span>                <span class="hl slc">;; this is inefficient as most of the stored</span>
<a id="l_1150"></a><span class="hl line"> 1150 </span>                <span class="hl slc">;; methods will not be for this particular gf n</span>
<a id="l_1151"></a><span class="hl line"> 1151 </span>                <span class="hl slc">;; But this is hardly performance-critical</span>
<a id="l_1152"></a><span class="hl line"> 1152 </span>                <span class="hl sym">(</span><span class="hl kwa">lambda</span> <span class="hl sym">(</span>m<span class="hl sym">)</span>
<a id="l_1153"></a><span class="hl line"> 1153 </span>                  <span class="hl sym">(</span>remove-method <span class="hl sym">(</span>symbol-function name<span class="hl sym">)</span> m<span class="hl sym">))</span>
<a id="l_1154"></a><span class="hl line"> 1154 </span>                <span class="hl sym">(</span>component-inline-methods ret<span class="hl sym">)))</span>
<a id="l_1155"></a><span class="hl line"> 1155 </span>  <span class="hl slc">;; clear methods, then add the new ones</span>
<a id="l_1156"></a><span class="hl line"> 1156 </span>  <span class="hl sym">(</span>setf <span class="hl sym">(</span>component-inline-methods ret<span class="hl sym">)</span> nil<span class="hl sym">)</span>
<a id="l_1157"></a><span class="hl line"> 1157 </span>  <span class="hl sym">(</span>loop for name in <span class="hl sym">+</span>asdf-methods<span class="hl sym">+</span>
<a id="l_1158"></a><span class="hl line"> 1158 </span>        for v <span class="hl sym">= (</span>getf rest <span class="hl sym">(</span>intern <span class="hl sym">(</span>symbol-name name<span class="hl sym">) :</span>keyword<span class="hl sym">))</span>
<a id="l_1159"></a><span class="hl line"> 1159 </span>        when v do
<a id="l_1160"></a><span class="hl line"> 1160 </span>        <span class="hl sym">(</span>destructuring-bind <span class="hl sym">(</span>op qual <span class="hl sym">(</span>o c<span class="hl sym">) &amp;</span>body body<span class="hl sym">)</span> v
<a id="l_1161"></a><span class="hl line"> 1161 </span>          <span class="hl sym">(</span>pushnew
<a id="l_1162"></a><span class="hl line"> 1162 </span>           <span class="hl sym">(</span><span class="hl kwa">eval</span> `<span class="hl sym">(</span>defmethod <span class="hl sym">,</span>name <span class="hl sym">,</span>qual <span class="hl sym">((,</span>o <span class="hl sym">,</span>op<span class="hl sym">) (,</span>c <span class="hl sym">(</span>eql <span class="hl sym">,</span>ret<span class="hl sym">)))</span>
<a id="l_1163"></a><span class="hl line"> 1163 </span>                             <span class="hl sym">,</span>&#64;body<span class="hl sym">))</span>
<a id="l_1164"></a><span class="hl line"> 1164 </span>           <span class="hl sym">(</span>component-inline-methods ret<span class="hl sym">)))))</span>
<a id="l_1165"></a><span class="hl line"> 1165 </span>
<a id="l_1166"></a><span class="hl line"> 1166 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> check-component-input <span class="hl sym">(</span><span class="hl kwa">type</span> name weakly-depends-on depends-on components in-order-to<span class="hl sym">)</span>
<a id="l_1167"></a><span class="hl line"> 1167 </span>  <span class="hl str">&quot;A partial test of the values of a component.&quot;</span>
<a id="l_1168"></a><span class="hl line"> 1168 </span>  <span class="hl sym">(</span>when weakly-depends-on <span class="hl sym">(</span>warn <span class="hl str">&quot;We got one! XXXXX&quot;</span><span class="hl sym">))</span>
<a id="l_1169"></a><span class="hl line"> 1169 </span>  <span class="hl sym">(</span>unless <span class="hl sym">(</span><span class="hl kwa">listp</span> depends-on<span class="hl sym">)</span>
<a id="l_1170"></a><span class="hl line"> 1170 </span>    <span class="hl sym">(</span>sysdef-error-component <span class="hl str">&quot;:depends-on must be a list.&quot;</span>
<a id="l_1171"></a><span class="hl line"> 1171 </span>                            <span class="hl kwa">type</span> name depends-on<span class="hl sym">))</span>
<a id="l_1172"></a><span class="hl line"> 1172 </span>  <span class="hl sym">(</span>unless <span class="hl sym">(</span><span class="hl kwa">listp</span> weakly-depends-on<span class="hl sym">)</span>
<a id="l_1173"></a><span class="hl line"> 1173 </span>    <span class="hl sym">(</span>sysdef-error-component <span class="hl str">&quot;:weakly-depends-on must be a list.&quot;</span>
<a id="l_1174"></a><span class="hl line"> 1174 </span>                            <span class="hl kwa">type</span> name weakly-depends-on<span class="hl sym">))</span>
<a id="l_1175"></a><span class="hl line"> 1175 </span>  <span class="hl sym">(</span>unless <span class="hl sym">(</span><span class="hl kwa">listp</span> components<span class="hl sym">)</span>
<a id="l_1176"></a><span class="hl line"> 1176 </span>    <span class="hl sym">(</span>sysdef-error-component <span class="hl str">&quot;:components must be NIL or a list of components.&quot;</span>
<a id="l_1177"></a><span class="hl line"> 1177 </span>                            <span class="hl kwa">type</span> name components<span class="hl sym">))</span>
<a id="l_1178"></a><span class="hl line"> 1178 </span>  <span class="hl sym">(</span>unless <span class="hl sym">(</span><span class="hl kwa">and</span> <span class="hl sym">(</span><span class="hl kwa">listp</span> in-order-to<span class="hl sym">) (</span><span class="hl kwa">listp</span> <span class="hl sym">(</span><span class="hl kwa">car</span> in-order-to<span class="hl sym">)))</span>
<a id="l_1179"></a><span class="hl line"> 1179 </span>    <span class="hl sym">(</span>sysdef-error-component <span class="hl str">&quot;:in-order-to must be NIL or a list of components.&quot;</span>
<a id="l_1180"></a><span class="hl line"> 1180 </span>                            <span class="hl kwa">type</span> name in-order-to<span class="hl sym">)))</span>
<a id="l_1181"></a><span class="hl line"> 1181 </span>
<a id="l_1182"></a><span class="hl line"> 1182 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> sysdef-error-component <span class="hl sym">(</span>msg <span class="hl kwa">type</span> name value<span class="hl sym">)</span>
<a id="l_1183"></a><span class="hl line"> 1183 </span>  <span class="hl sym">(</span>sysdef-error <span class="hl sym">(</span>concatenate <span class="hl sym">'</span>string msg
<a id="l_1184"></a><span class="hl line"> 1184 </span>                             <span class="hl str">&quot;~&amp;The value specified for ~(~A~) ~A is ~W&quot;</span><span class="hl sym">)</span>
<a id="l_1185"></a><span class="hl line"> 1185 </span>                <span class="hl kwa">type</span> name value<span class="hl sym">))</span>
<a id="l_1186"></a><span class="hl line"> 1186 </span>
<a id="l_1187"></a><span class="hl line"> 1187 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> resolve-symlinks <span class="hl sym">(</span>path<span class="hl sym">)</span>
<a id="l_1188"></a><span class="hl line"> 1188 </span>  #-allegro <span class="hl sym">(</span>truename path<span class="hl sym">)</span>
<a id="l_1189"></a><span class="hl line"> 1189 </span>  #<span class="hl sym">+</span>allegro <span class="hl sym">(</span>excl<span class="hl sym">:</span>pathname-resolve-symbolic-links path<span class="hl sym">)</span>
<a id="l_1190"></a><span class="hl line"> 1190 </span>  <span class="hl sym">)</span>
<a id="l_1191"></a><span class="hl line"> 1191 </span>
<a id="l_1192"></a><span class="hl line"> 1192 </span><span class="hl slc">;;; optional extras</span>
<a id="l_1193"></a><span class="hl line"> 1193 </span>
<a id="l_1194"></a><span class="hl line"> 1194 </span><span class="hl slc">;;; run-shell-command functions for other lisp implementations will be</span>
<a id="l_1195"></a><span class="hl line"> 1195 </span><span class="hl slc">;;; gratefully accepted, if they do the same thing.  If the docstring</span>
<a id="l_1196"></a><span class="hl line"> 1196 </span><span class="hl slc">;;; is ambiguous, send a bug report</span>
<a id="l_1197"></a><span class="hl line"> 1197 </span>
<a id="l_1198"></a><span class="hl line"> 1198 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> run-shell-<span class="hl kwa">command</span> <span class="hl sym">(</span>control-string <span class="hl sym">&amp;</span>rest args<span class="hl sym">)</span>
<a id="l_1199"></a><span class="hl line"> 1199 </span>  <span class="hl str">&quot;Interpolate ARGS into CONTROL-STRING as if by FORMAT, and</span>
<a id="l_1200"></a><span class="hl line"> 1200 </span><span class="hl str">synchronously execute the result using a Bourne-compatible shell, with</span>
<a id="l_1201"></a><span class="hl line"> 1201 </span><span class="hl str">output to *VERBOSE-OUT*.  Returns the shell's exit code.&quot;</span>
<a id="l_1202"></a><span class="hl line"> 1202 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span><span class="hl kwa">command</span> <span class="hl sym">(</span><span class="hl kwa">apply</span> #<span class="hl sym">'</span>format nil control-string args<span class="hl sym">)))</span>
<a id="l_1203"></a><span class="hl line"> 1203 </span>    <span class="hl sym">(</span>format <span class="hl sym">*</span>verbose-out<span class="hl sym">*</span> <span class="hl str">&quot;; $ ~A~%&quot;</span> <span class="hl kwa">command</span><span class="hl sym">)</span>
<a id="l_1204"></a><span class="hl line"> 1204 </span>    #<span class="hl sym">+</span>sbcl
<a id="l_1205"></a><span class="hl line"> 1205 </span>    <span class="hl sym">(</span>sb-ext<span class="hl sym">:</span>process-<span class="hl kwa">exit</span>-code
<a id="l_1206"></a><span class="hl line"> 1206 </span>     <span class="hl sym">(</span>sb-ext<span class="hl sym">:</span>run-program
<a id="l_1207"></a><span class="hl line"> 1207 </span>      #<span class="hl sym">+</span>win32 <span class="hl str">&quot;sh&quot;</span> #-win32 <span class="hl str">&quot;/bin/sh&quot;</span>
<a id="l_1208"></a><span class="hl line"> 1208 </span>      <span class="hl sym">(</span><span class="hl kwa">list</span>  <span class="hl str">&quot;-c&quot;</span> <span class="hl kwa">command</span><span class="hl sym">)</span>
<a id="l_1209"></a><span class="hl line"> 1209 </span>      #<span class="hl sym">+</span>win32 #<span class="hl sym">+</span>win32 <span class="hl sym">:</span>search t
<a id="l_1210"></a><span class="hl line"> 1210 </span>      <span class="hl sym">:</span>input nil <span class="hl sym">:</span>output <span class="hl sym">*</span>verbose-out<span class="hl sym">*))</span>
<a id="l_1211"></a><span class="hl line"> 1211 </span>
<a id="l_1212"></a><span class="hl line"> 1212 </span>    #<span class="hl sym">+(</span><span class="hl kwa">or</span> cmu scl<span class="hl sym">)</span>
<a id="l_1213"></a><span class="hl line"> 1213 </span>    <span class="hl sym">(</span>ext<span class="hl sym">:</span>process-<span class="hl kwa">exit</span>-code
<a id="l_1214"></a><span class="hl line"> 1214 </span>     <span class="hl sym">(</span>ext<span class="hl sym">:</span>run-program
<a id="l_1215"></a><span class="hl line"> 1215 </span>      <span class="hl str">&quot;/bin/sh&quot;</span>
<a id="l_1216"></a><span class="hl line"> 1216 </span>      <span class="hl sym">(</span><span class="hl kwa">list</span>  <span class="hl str">&quot;-c&quot;</span> <span class="hl kwa">command</span><span class="hl sym">)</span>
<a id="l_1217"></a><span class="hl line"> 1217 </span>      <span class="hl sym">:</span>input nil <span class="hl sym">:</span>output <span class="hl sym">*</span>verbose-out<span class="hl sym">*))</span>
<a id="l_1218"></a><span class="hl line"> 1218 </span>
<a id="l_1219"></a><span class="hl line"> 1219 </span>    #<span class="hl sym">+</span>allegro
<a id="l_1220"></a><span class="hl line"> 1220 </span>    <span class="hl sym">(</span>excl<span class="hl sym">:</span>run-shell-<span class="hl kwa">command command</span> <span class="hl sym">:</span>input nil <span class="hl sym">:</span>output <span class="hl sym">*</span>verbose-out<span class="hl sym">*)</span>
<a id="l_1221"></a><span class="hl line"> 1221 </span>
<a id="l_1222"></a><span class="hl line"> 1222 </span>    #<span class="hl sym">+</span>lispworks
<a id="l_1223"></a><span class="hl line"> 1223 </span>    <span class="hl sym">(</span>system<span class="hl sym">:</span>call-system-showing-output
<a id="l_1224"></a><span class="hl line"> 1224 </span>     <span class="hl kwa">command</span>
<a id="l_1225"></a><span class="hl line"> 1225 </span>     <span class="hl sym">:</span>shell-<span class="hl kwa">type</span> <span class="hl str">&quot;/bin/sh&quot;</span>
<a id="l_1226"></a><span class="hl line"> 1226 </span>     <span class="hl sym">:</span>output-stream <span class="hl sym">*</span>verbose-out<span class="hl sym">*)</span>
<a id="l_1227"></a><span class="hl line"> 1227 </span>
<a id="l_1228"></a><span class="hl line"> 1228 </span>    #<span class="hl sym">+</span>clisp                     <span class="hl slc">;XXX not exactly *verbose-out*, I know</span>
<a id="l_1229"></a><span class="hl line"> 1229 </span>    <span class="hl sym">(</span>ext<span class="hl sym">:</span>run-shell-<span class="hl kwa">command  command</span> <span class="hl sym">:</span>output <span class="hl sym">:</span>terminal <span class="hl sym">:</span>wait t<span class="hl sym">)</span>
<a id="l_1230"></a><span class="hl line"> 1230 </span>
<a id="l_1231"></a><span class="hl line"> 1231 </span>    #<span class="hl sym">+</span>openmcl
<a id="l_1232"></a><span class="hl line"> 1232 </span>    <span class="hl sym">(</span><span class="hl kwa">nth</span>-value <span class="hl num">1</span>
<a id="l_1233"></a><span class="hl line"> 1233 </span>               <span class="hl sym">(</span>ccl<span class="hl sym">:</span>external-process-status
<a id="l_1234"></a><span class="hl line"> 1234 </span>                <span class="hl sym">(</span>ccl<span class="hl sym">:</span>run-program <span class="hl str">&quot;/bin/sh&quot;</span> <span class="hl sym">(</span><span class="hl kwa">list</span> <span class="hl str">&quot;-c&quot;</span> <span class="hl kwa">command</span><span class="hl sym">)</span>
<a id="l_1235"></a><span class="hl line"> 1235 </span>                                 <span class="hl sym">:</span>input nil <span class="hl sym">:</span>output <span class="hl sym">*</span>verbose-out<span class="hl sym">*</span>
<a id="l_1236"></a><span class="hl line"> 1236 </span>                                 <span class="hl sym">:</span>wait t<span class="hl sym">)))</span>
<a id="l_1237"></a><span class="hl line"> 1237 </span>    #<span class="hl sym">+</span>ecl <span class="hl slc">;; courtesy of Juan Jose Garcia Ripoll</span>
<a id="l_1238"></a><span class="hl line"> 1238 </span>    <span class="hl sym">(</span>si<span class="hl sym">:</span>system <span class="hl kwa">command</span><span class="hl sym">)</span>
<a id="l_1239"></a><span class="hl line"> 1239 </span>    #-<span class="hl sym">(</span><span class="hl kwa">or</span> openmcl clisp lispworks allegro scl cmu sbcl ecl<span class="hl sym">)</span>
<a id="l_1240"></a><span class="hl line"> 1240 </span>    <span class="hl sym">(</span>error <span class="hl str">&quot;RUN-SHELL-PROGRAM not implemented for this Lisp&quot;</span><span class="hl sym">)</span>
<a id="l_1241"></a><span class="hl line"> 1241 </span>    <span class="hl sym">))</span>
<a id="l_1242"></a><span class="hl line"> 1242 </span>
<a id="l_1243"></a><span class="hl line"> 1243 </span>
<a id="l_1244"></a><span class="hl line"> 1244 </span><span class="hl sym">(</span>defgeneric hyperdocumentation <span class="hl sym">(</span>package name doc-<span class="hl kwa">type</span><span class="hl sym">))</span>
<a id="l_1245"></a><span class="hl line"> 1245 </span><span class="hl sym">(</span>defmethod hyperdocumentation <span class="hl sym">((</span>package symbol<span class="hl sym">)</span> name doc-<span class="hl kwa">type</span><span class="hl sym">)</span>
<a id="l_1246"></a><span class="hl line"> 1246 </span>  <span class="hl sym">(</span>hyperdocumentation <span class="hl sym">(</span>find-package package<span class="hl sym">)</span> name doc-<span class="hl kwa">type</span><span class="hl sym">))</span>
<a id="l_1247"></a><span class="hl line"> 1247 </span>
<a id="l_1248"></a><span class="hl line"> 1248 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> hyperdoc <span class="hl sym">(</span>name doc-<span class="hl kwa">type</span><span class="hl sym">)</span>
<a id="l_1249"></a><span class="hl line"> 1249 </span>  <span class="hl sym">(</span>hyperdocumentation <span class="hl sym">(</span>symbol-package name<span class="hl sym">)</span> name doc-<span class="hl kwa">type</span><span class="hl sym">))</span>
<a id="l_1250"></a><span class="hl line"> 1250 </span>
<a id="l_1251"></a><span class="hl line"> 1251 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> system-source-file <span class="hl sym">(</span>system-name<span class="hl sym">)</span>
<a id="l_1252"></a><span class="hl line"> 1252 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>system <span class="hl sym">(</span>asdf<span class="hl sym">:</span>find-system system-name<span class="hl sym">)))</span>
<a id="l_1253"></a><span class="hl line"> 1253 </span>    <span class="hl sym">(</span>make-pathname
<a id="l_1254"></a><span class="hl line"> 1254 </span>     <span class="hl sym">:</span><span class="hl kwa">type</span> <span class="hl str">&quot;asd&quot;</span>
<a id="l_1255"></a><span class="hl line"> 1255 </span>     <span class="hl sym">:</span>name <span class="hl sym">(</span>asdf<span class="hl sym">:</span>component-name system<span class="hl sym">)</span>
<a id="l_1256"></a><span class="hl line"> 1256 </span>     <span class="hl sym">:</span>defaults <span class="hl sym">(</span>asdf<span class="hl sym">:</span>component-relative-pathname system<span class="hl sym">))))</span>
<a id="l_1257"></a><span class="hl line"> 1257 </span>
<a id="l_1258"></a><span class="hl line"> 1258 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> system-source-directory <span class="hl sym">(</span>system-name<span class="hl sym">)</span>
<a id="l_1259"></a><span class="hl line"> 1259 </span>  <span class="hl sym">(</span>make-pathname <span class="hl sym">:</span>name nil
<a id="l_1260"></a><span class="hl line"> 1260 </span>                 <span class="hl sym">:</span><span class="hl kwa">type</span> nil
<a id="l_1261"></a><span class="hl line"> 1261 </span>                 <span class="hl sym">:</span>defaults <span class="hl sym">(</span>system-source-file system-name<span class="hl sym">)))</span>
<a id="l_1262"></a><span class="hl line"> 1262 </span>
<a id="l_1263"></a><span class="hl line"> 1263 </span><span class="hl sym">(</span><span class="hl kwa">defun</span> system-relative-pathname <span class="hl sym">(</span>system pathname <span class="hl sym">&amp;</span>key name <span class="hl kwa">type</span><span class="hl sym">)</span>
<a id="l_1264"></a><span class="hl line"> 1264 </span>  <span class="hl sym">(</span>let <span class="hl sym">((</span>directory <span class="hl sym">(</span>pathname-directory pathname<span class="hl sym">)))</span>
<a id="l_1265"></a><span class="hl line"> 1265 </span>    <span class="hl sym">(</span>when <span class="hl sym">(</span><span class="hl kwa">eq</span> <span class="hl sym">(</span><span class="hl kwa">car</span> directory<span class="hl sym">) :</span>absolute<span class="hl sym">)</span>
<a id="l_1266"></a><span class="hl line"> 1266 </span>      <span class="hl sym">(</span>setf <span class="hl sym">(</span><span class="hl kwa">car</span> directory<span class="hl sym">) :</span>relative<span class="hl sym">))</span>
<a id="l_1267"></a><span class="hl line"> 1267 </span>    <span class="hl sym">(</span>merge-pathnames
<a id="l_1268"></a><span class="hl line"> 1268 </span>     <span class="hl sym">(</span>make-pathname <span class="hl sym">:</span>name <span class="hl sym">(</span><span class="hl kwa">or</span> name <span class="hl sym">(</span>pathname-name pathname<span class="hl sym">))</span>
<a id="l_1269"></a><span class="hl line"> 1269 </span>                    <span class="hl sym">:</span><span class="hl kwa">type</span> <span class="hl sym">(</span><span class="hl kwa">or type</span> <span class="hl sym">(</span>pathname-<span class="hl kwa">type</span> pathname<span class="hl sym">))</span>
<a id="l_1270"></a><span class="hl line"> 1270 </span>                    <span class="hl sym">:</span>directory directory<span class="hl sym">)</span>
<a id="l_1271"></a><span class="hl line"> 1271 </span>     <span class="hl sym">(</span>system-source-directory system<span class="hl sym">))))</span>
<a id="l_1272"></a><span class="hl line"> 1272 </span>
<a id="l_1273"></a><span class="hl line"> 1273 </span>
<a id="l_1274"></a><span class="hl line"> 1274 </span><span class="hl sym">(</span>pushnew <span class="hl sym">:</span>asdf <span class="hl sym">*</span>features<span class="hl sym">*)</span>
<a id="l_1275"></a><span class="hl line"> 1275 </span>
<a id="l_1276"></a><span class="hl line"> 1276 </span>#<span class="hl sym">+</span>sbcl
<a id="l_1277"></a><span class="hl line"> 1277 </span><span class="hl sym">(</span><span class="hl kwa">eval</span>-when <span class="hl sym">(:</span>compile-toplevel <span class="hl sym">:</span><span class="hl kwa">load</span>-toplevel <span class="hl sym">:</span>execute<span class="hl sym">)</span>
<a id="l_1278"></a><span class="hl line"> 1278 </span>  <span class="hl sym">(</span>when <span class="hl sym">(</span>sb-ext<span class="hl sym">:</span>posix-<span class="hl kwa">getenv</span> <span class="hl str">&quot;SBCL_BUILDING_CONTRIB&quot;</span><span class="hl sym">)</span>
<a id="l_1279"></a><span class="hl line"> 1279 </span>    <span class="hl sym">(</span>pushnew <span class="hl sym">:</span>sbcl-hooks-require <span class="hl sym">*</span>features<span class="hl sym">*)))</span>
<a id="l_1280"></a><span class="hl line"> 1280 </span>
<a id="l_1281"></a><span class="hl line"> 1281 </span>#<span class="hl sym">+(</span><span class="hl kwa">and</span> sbcl sbcl-hooks-require<span class="hl sym">)</span>
<a id="l_1282"></a><span class="hl line"> 1282 </span><span class="hl sym">(</span><span class="hl kwa">progn</span>
<a id="l_1283"></a><span class="hl line"> 1283 </span>  <span class="hl sym">(</span><span class="hl kwa">defun</span> module-provide-asdf <span class="hl sym">(</span>name<span class="hl sym">)</span>
<a id="l_1284"></a><span class="hl line"> 1284 </span>    <span class="hl sym">(</span>handler-bind <span class="hl sym">((</span>style-warning #<span class="hl sym">'</span>muffle-warning<span class="hl sym">))</span>
<a id="l_1285"></a><span class="hl line"> 1285 </span>      <span class="hl sym">(</span>let<span class="hl sym">* ((*</span>verbose-out<span class="hl sym">* (</span>make-broadcast-stream<span class="hl sym">))</span>
<a id="l_1286"></a><span class="hl line"> 1286 </span>             <span class="hl sym">(</span>system <span class="hl sym">(</span>asdf<span class="hl sym">:</span>find-system name nil<span class="hl sym">)))</span>
<a id="l_1287"></a><span class="hl line"> 1287 </span>        <span class="hl sym">(</span>when system
<a id="l_1288"></a><span class="hl line"> 1288 </span>          <span class="hl sym">(</span>asdf<span class="hl sym">:</span>operate <span class="hl sym">'</span>asdf<span class="hl sym">:</span><span class="hl kwa">load</span>-op name<span class="hl sym">)</span>
<a id="l_1289"></a><span class="hl line"> 1289 </span>          t<span class="hl sym">))))</span>
<a id="l_1290"></a><span class="hl line"> 1290 </span>
<a id="l_1291"></a><span class="hl line"> 1291 </span>  <span class="hl sym">(</span><span class="hl kwa">defun</span> contrib-sysdef-search <span class="hl sym">(</span>system<span class="hl sym">)</span>
<a id="l_1292"></a><span class="hl line"> 1292 </span>    <span class="hl sym">(</span>let <span class="hl sym">((</span>home <span class="hl sym">(</span>sb-ext<span class="hl sym">:</span>posix-<span class="hl kwa">getenv</span> <span class="hl str">&quot;SBCL_HOME&quot;</span><span class="hl sym">)))</span>
<a id="l_1293"></a><span class="hl line"> 1293 </span>      <span class="hl sym">(</span>when <span class="hl sym">(</span><span class="hl kwa">and</span> home <span class="hl sym">(</span><span class="hl kwa">not</span> <span class="hl sym">(</span>string<span class="hl sym">=</span> home <span class="hl str">&quot;&quot;</span><span class="hl sym">)))</span>
<a id="l_1294"></a><span class="hl line"> 1294 </span>        <span class="hl sym">(</span>let<span class="hl sym">* ((</span>name <span class="hl sym">(</span>coerce-name system<span class="hl sym">))</span>
<a id="l_1295"></a><span class="hl line"> 1295 </span>               <span class="hl sym">(</span>home <span class="hl sym">(</span>truename home<span class="hl sym">))</span>
<a id="l_1296"></a><span class="hl line"> 1296 </span>               <span class="hl sym">(</span>contrib <span class="hl sym">(</span>merge-pathnames
<a id="l_1297"></a><span class="hl line"> 1297 </span>                         <span class="hl sym">(</span>make-pathname <span class="hl sym">:</span>directory `<span class="hl sym">(:</span>relative <span class="hl sym">,</span>name<span class="hl sym">)</span>
<a id="l_1298"></a><span class="hl line"> 1298 </span>                                        <span class="hl sym">:</span>name name
<a id="l_1299"></a><span class="hl line"> 1299 </span>                                        <span class="hl sym">:</span><span class="hl kwa">type</span> <span class="hl str">&quot;asd&quot;</span>
<a id="l_1300"></a><span class="hl line"> 1300 </span>                                        <span class="hl sym">:</span>case <span class="hl sym">:</span>local
<a id="l_1301"></a><span class="hl line"> 1301 </span>                                        <span class="hl sym">:</span>version <span class="hl sym">:</span>newest<span class="hl sym">)</span>
<a id="l_1302"></a><span class="hl line"> 1302 </span>                         home<span class="hl sym">)))</span>
<a id="l_1303"></a><span class="hl line"> 1303 </span>          <span class="hl sym">(</span>probe-file contrib<span class="hl sym">)))))</span>
<a id="l_1304"></a><span class="hl line"> 1304 </span>
<a id="l_1305"></a><span class="hl line"> 1305 </span>  <span class="hl sym">(</span>pushnew
<a id="l_1306"></a><span class="hl line"> 1306 </span>   <span class="hl sym">'(</span>let <span class="hl sym">((</span>home <span class="hl sym">(</span>sb-ext<span class="hl sym">:</span>posix-<span class="hl kwa">getenv</span> <span class="hl str">&quot;SBCL_HOME&quot;</span><span class="hl sym">)))</span>
<a id="l_1307"></a><span class="hl line"> 1307 </span>      <span class="hl sym">(</span>when <span class="hl sym">(</span><span class="hl kwa">and</span> home <span class="hl sym">(</span><span class="hl kwa">not</span> <span class="hl sym">(</span>string<span class="hl sym">=</span> home <span class="hl str">&quot;&quot;</span><span class="hl sym">)))</span>
<a id="l_1308"></a><span class="hl line"> 1308 </span>        <span class="hl sym">(</span>merge-pathnames <span class="hl str">&quot;site-systems/&quot;</span> <span class="hl sym">(</span>truename home<span class="hl sym">))))</span>
<a id="l_1309"></a><span class="hl line"> 1309 </span>   <span class="hl sym">*</span>central-registry<span class="hl sym">*)</span>
<a id="l_1310"></a><span class="hl line"> 1310 </span>
<a id="l_1311"></a><span class="hl line"> 1311 </span>  <span class="hl sym">(</span>pushnew
<a id="l_1312"></a><span class="hl line"> 1312 </span>   <span class="hl sym">'(</span>merge-pathnames <span class="hl str">&quot;.sbcl/systems/&quot;</span>
<a id="l_1313"></a><span class="hl line"> 1313 </span>     <span class="hl sym">(</span>user-homedir-pathname<span class="hl sym">))</span>
<a id="l_1314"></a><span class="hl line"> 1314 </span>   <span class="hl sym">*</span>central-registry<span class="hl sym">*)</span>
<a id="l_1315"></a><span class="hl line"> 1315 </span>
<a id="l_1316"></a><span class="hl line"> 1316 </span>  <span class="hl sym">(</span>pushnew <span class="hl sym">'</span>module-provide-asdf sb-ext<span class="hl sym">:*</span>module-provider-functions<span class="hl sym">*)</span>
<a id="l_1317"></a><span class="hl line"> 1317 </span>  <span class="hl sym">(</span>pushnew <span class="hl sym">'</span>contrib-sysdef-search <span class="hl sym">*</span>system-definition-search-functions<span class="hl sym">*))</span>
<a id="l_1318"></a><span class="hl line"> 1318 </span>
<a id="l_1319"></a><span class="hl line"> 1319 </span><span class="hl sym">(</span>provide <span class="hl sym">'</span>asdf<span class="hl sym">)</span>
</pre></div>

<hr />
<table>
<tr>
<td>
<address><a href="http://sourceforge.net/">Back to SourceForge.net</a></address><br />
Powered by <a href="http://viewvc.tigris.org/">ViewVC 1.0.3</a>
</td>
<td style="text-align:right;">
<h3><a href="/*docroot*/help_rootview.html">ViewVC and Help</a></h3>
</td>
</tr>
</table>
</body>
</html>

