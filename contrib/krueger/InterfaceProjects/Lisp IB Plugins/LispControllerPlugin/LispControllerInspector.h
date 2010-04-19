//
//  LispControllerInspector.h
//  LispControllerPlugin
//
//  Created by Paul Krueger on 2/5/10.
//  Copyright 2010. All rights reserved.
//

#import <InterfaceBuilderKit/InterfaceBuilderKit.h>
#import <LispController.h>

@interface LispControllerInspector : IBInspector {
	IBOutlet NSTableView *typeTable;
	IBOutlet NSTableView *initformTable;
	IBOutlet NSTableView *sortTable;
}


@end
