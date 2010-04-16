//
//  LispIntegration.m
//  LispControllerPlugin
//
//  Created by Paul Krueger on 2/8/10.
//  Copyright 2010. All rights reserved.
//

#import <InterfaceBuilderKit/InterfaceBuilderKit.h>
#import <LispControllerInspector.h>
#import <LispController.h>


@implementation LispController ( LispController )

- (void)ibPopulateKeyPaths:(NSMutableDictionary *)keyPaths {
    [super ibPopulateKeyPaths:keyPaths];
	
	//  KVC-compliant properties.
    [[keyPaths objectForKey:IBAttributeKeyPaths] addObjectsFromArray:[NSArray arrayWithObjects:
																	  @"rootType",
																	  @"readerFunc",
																	  @"writerFunc",
																	  @"countFunc",
																	  @"selectFunc",
																	  @"editedFunc",
																	  @"addedFunc",
																	  @"insertFunc",
																	  @"deleteFunc",
																	  @"addChildFunc",
																	  @"childrenFunc",
																	  @"maxDepth",
																	  @"genRoot",
																	  nil]];
/*
	[[keyPaths objectForKey:IBToManyRelationshipKeyPaths] addObjectsFromArray:[NSArray arrayWithObjects:
																			   @"typeInfo",
																			   @"initforms",
																			   @"sortInfo",
																			   nil]];
 */
}

- (void)ibPopulateAttributeInspectorClasses:(NSMutableArray *)classes {
    [super ibPopulateAttributeInspectorClasses:classes];
    [classes addObject:[LispControllerInspector class]];
}


@end
