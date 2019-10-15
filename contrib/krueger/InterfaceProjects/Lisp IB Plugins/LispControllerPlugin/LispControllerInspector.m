//
//  LispControllerInspector.m
//  LispControllerPlugin
//
//  Created by Paul Krueger on 2/5/10.
//  Copyright 2010. All rights reserved.
//

#import "LispControllerInspector.h"

@implementation LispControllerInspector

- (BOOL)supportsMultipleObjectInspection {
	return NO;
}

- (NSString *)viewNibName {
    return @"LispControllerInspector";
}

- (void)refresh {
	// Synchronize your inspector's content view with the currently selected objects
	[super refresh];
	[typeTable reloadData];
	[initformTable reloadData];
	[sortTable reloadData];
}

- (int)numberOfRowsInTableView:(NSTableView *)tab {
	NSArray* objects = [self inspectedObjects];
	NSInteger numObjects = [objects count];
	if (numObjects == 1) {
		LispController *lc = [objects objectAtIndex:0];
		if (tab == typeTable) {
			return [lc typeInfoCount];
		} else if (tab == initformTable) {
			return [lc initformCount];
		} else if (tab == sortTable) {
			return [lc sortInfoCount];
		}
	}
	return 0;
}

- (id) tableView: (NSTableView *)tab
	objectValueForTableColumn: (NSTableColumn *)colObj
			 row: (int)rowIndex {
	NSArray* objects = [self inspectedObjects];
	NSInteger numObjects = [objects count];
	if (numObjects == 1) {
		LispController *lc = [objects objectAtIndex:0];
		int colIndex = [[colObj identifier] intValue];
		if (tab == typeTable) {
			return [lc typeTableCellAtRow:rowIndex col:colIndex];
		} else if (tab == initformTable) {
			return [lc initformTableCellAtRow:rowIndex col:colIndex];
		} else if (tab == sortTable) {
			return [lc sortTableCellAtRow:rowIndex col:colIndex];
		}
	}
	return @"";
}

- (void) tableView: (NSTableView *) tab
	setObjectValue: (id) newVal
	forTableColumn: (NSTableColumn *) colObj
			   row: (int) rowIndex {
	NSArray* objects = [self inspectedObjects];
	NSInteger numObjects = [objects count];
	if (numObjects == 1) {
		LispController *lc = [objects objectAtIndex:0];
		int colIndex = [[colObj identifier] intValue];
		int rowCount;
		if (tab == typeTable) {
			rowCount = [lc typeInfoCount];
			[lc setValue:newVal forTypeTableCellAtRow:rowIndex col:colIndex];
			if ((rowIndex == (rowCount - 1)) &&
				![newVal isEqualToString:@""]) {
				[lc addTypeRow];
				[tab reloadData];
			}
			if ((rowIndex < (rowCount - 1)) &&
				[[lc typeTableCellAtRow:rowIndex col:0] isEqualToString:@""] &&
				[[lc typeTableCellAtRow:rowIndex col:1] isEqualToString:@""] &&
				[[lc typeTableCellAtRow:rowIndex col:2] isEqualToString:@""]) {
				[lc  typeTableRemoveRow: rowIndex];
				[tab reloadData];
			}
		} else if (tab == initformTable) {
			rowCount = [lc initformCount];
			[lc setValue:newVal forInitformTableCellAtRow:rowIndex col:colIndex];
			if ((rowIndex == (rowCount - 1)) &&
				![newVal isEqualToString:@""]) {
				[lc addInitformRow];
				[tab reloadData];
			}
			if ((rowIndex < (rowCount - 1)) &&
				[[lc initformTableCellAtRow:rowIndex col:0] isEqualToString:@""] &&
				[[lc initformTableCellAtRow:rowIndex col:1] isEqualToString:@""]) {
				[lc  initformTableRemoveRow: rowIndex];
				[tab reloadData];
			}					
		} else if (tab == sortTable) {
			rowCount = [lc sortInfoCount];
			[lc setValue:newVal forSortTableCellAtRow:rowIndex col:colIndex];
			if ((rowIndex == (rowCount - 1)) &&
				![newVal isEqualToString:@""]) {
				[lc addSortRow];
				[tab reloadData];
			}
			if ((rowIndex < (rowCount - 1)) &&
				[[lc sortTableCellAtRow:rowIndex col:0] isEqualToString:@""] &&
				[[lc sortTableCellAtRow:rowIndex col:1] isEqualToString:@""] &&
				[[lc sortTableCellAtRow:rowIndex col:2] isEqualToString:@""]) {
				[lc  sortTableRemoveRow: rowIndex];
				[tab reloadData];
			}	
		}
	}
}	

@end
