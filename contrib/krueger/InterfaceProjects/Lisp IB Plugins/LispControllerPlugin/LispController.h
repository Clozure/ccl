//
//  LispController.h
//  LispControllerPlugin
//
//  Created by Paul Krueger on 2/5/10.
//  Copyright 2010. All rights reserved.
//


@interface LispController : NSObject {
	NSMutableArray *typeInfo;
	NSMutableArray *initforms;
	NSMutableArray *sortInfo;
	NSString *rootType;
	NSString *readerFunc;
	NSString *writerFunc;
	NSString *countFunc;
	NSString *selectFunc;
	NSString *editedFunc;
	NSString *addedFunc;
	NSString *removedFunc;
	NSString *deleteFunc;
	NSString *addChildFunc;
	NSString *childrenFunc;
	BOOL genRoot;
	IBOutlet NSView *view;
	IBOutlet id *owner;
}
- (void)encodeWithCoder:(NSCoder *)encoder;
- (id)initWithCoder:(NSCoder *)decoder;
- (id)init;
- (int)typeInfoCount;
- (int)initformCount;
- (int)sortInfoCount;
- (void)addTypeRow;
- (void)addInitformRow;
- (void)addSortRow;
- (void)typeTableRemoveRow: (int)row;
- (void)initformTableRemoveRow: (int)row;
- (void)sortTableRemoveRow: (int)row;
- (id) emptyArrayOfSize: (int) sz;
- (id) typeTableCellAtRow:(int)row col:(int)col;
- (id) initformTableCellAtRow:(int)row col:(int)col;
- (id) sortTableCellAtRow:(int)row col:(int)col;
- (void) setValue:(id)newVal forTypeTableCellAtRow:(int)rowIndex col:(int)colIndex;
- (void) setValue:(id)newVal forInitformTableCellAtRow:(int)rowIndex col:(int)colIndex;
- (void) setValue:(id)newVal forSortTableCellAtRow:(int)rowIndex col:(int)colIndex;
- (IBAction)insert: (id)sender;
- (IBAction)addChild: (id)sender;
- (IBAction)remove: (id)sender;

@end
