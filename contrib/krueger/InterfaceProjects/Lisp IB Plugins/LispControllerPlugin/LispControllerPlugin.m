//
//  LispControllerPlugin.m
//  LispControllerPlugin
//
//  Created by Paul Krueger on 2/3/10.
//  Copyright. All rights reserved.
//

#import "LispControllerPlugin.h"

@implementation LispControllerPlugin
- (NSArray *)libraryNibNames {
    return [NSArray arrayWithObject:@"LispControllerPluginLibrary"];
}

- (NSArray *)requiredFrameworks {
    return [NSArray arrayWithObjects:[NSBundle bundleWithIdentifier:@"com.yourcompany.LispControllerPlugin"], nil];
}

@end
