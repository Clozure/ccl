//
//  LispController.m
//  LispControllerPlugin
//
//  Created by Paul Krueger on 2/4/10.
//  Copyright 2010. All rights reserved.
//

#import <LispController.h>

@implementation LispController

- (void)encodeWithCoder:(NSCoder *)encoder {
	[encoder encodeObject:typeInfo forKey:@"typeInfo"];
	[encoder encodeObject:initforms forKey:@"initforms"];
	[encoder encodeObject:sortInfo forKey:@"sortInfo"];
	[encoder encodeObject:rootType forKey:@"rootType"];
	[encoder encodeObject:readerFunc forKey:@"readerFunc"];
	[encoder encodeObject:writerFunc forKey:@"writerFunc"];
	[encoder encodeObject:countFunc forKey:@"countFunc"];
	[encoder encodeObject:selectFunc forKey:@"selectFunc"];
	[encoder encodeObject:editedFunc forKey:@"editedFunc"];
	[encoder encodeObject:addedFunc forKey:@"addedFunc"];
	[encoder encodeObject:removedFunc forKey:@"removedFunc"];
	[encoder encodeObject:deleteFunc forKey:@"deleteFunc"];
	[encoder encodeObject:addChildFunc forKey:@"addChildFunc"];
	[encoder encodeObject:childrenFunc forKey:@"childrenFunc"];
	[encoder encodeBool:genRoot forKey:@"genRoot"];
}

- (id)init {
	typeInfo = [[NSMutableArray alloc] initWithCapacity:5];
	initforms = [[NSMutableArray alloc] initWithCapacity:5];
	sortInfo = [[NSMutableArray alloc] initWithCapacity:5];
	rootType = @"";
	readerFunc = @"";
	writerFunc = @"";
	countFunc = @"";
	selectFunc = @"";
	editedFunc = @"";
	deleteFunc = @"";
	addChildFunc = @"";
	childrenFunc = @"";
	genRoot = YES;
	
	[self addTypeRow];
	[self addInitformRow];
	[self addSortRow];
	return self;
}

- (id)initWithCoder:(NSCoder *)decoder {
	typeInfo = [[decoder decodeObjectForKey:@"typeInfo"] retain];
	initforms = [[decoder decodeObjectForKey:@"initforms"] retain];
	sortInfo = [[decoder decodeObjectForKey:@"sortInfo"] retain];
	rootType = [[decoder decodeObjectForKey:@"rootType"] retain];
	readerFunc = [[decoder decodeObjectForKey:@"readerFunc"] retain];
	writerFunc = [[decoder decodeObjectForKey:@"writerFunc"] retain];
	countFunc = [[decoder decodeObjectForKey:@"countFunc"] retain];
	selectFunc = [[decoder decodeObjectForKey:@"selectFunc"] retain];
	editedFunc = [[decoder decodeObjectForKey:@"editedFunc"] retain];
	addedFunc = [[decoder decodeObjectForKey:@"addedFunc"] retain];
	removedFunc = [[decoder decodeObjectForKey:@"removedFunc"] retain];
	deleteFunc = [[decoder decodeObjectForKey:@"deleteFunc"] retain];
	addChildFunc = [[decoder decodeObjectForKey:@"addChildFunc"] retain];
	childrenFunc = [[decoder decodeObjectForKey:@"childrenFunc"] retain];
	genRoot = [decoder decodeBoolForKey:@"genRoot"];
	return self;
}

- (int)typeInfoCount {
	return [typeInfo count];
}

- (int)initformCount {
	return [initforms count];
}

- (int)sortInfoCount {
	return [sortInfo count];
}


- (void)addTypeRow {
	int index = [typeInfo count];
	NSMutableArray *firstTypeEntry = [self emptyArrayOfSize:3];
	[typeInfo insertObject:firstTypeEntry atIndex: index];
}

- (void)addInitformRow {
	int index = [initforms count];
	NSMutableArray *firstInitformEntry = [self emptyArrayOfSize:2];
	[initforms insertObject:firstInitformEntry atIndex: index];
}

- (void)addSortRow {
	int index = [sortInfo count];
	NSMutableArray *firstSortEntry = [self emptyArrayOfSize:3];
	[sortInfo insertObject:firstSortEntry atIndex: index];
}

- (void)typeTableRemoveRow: (int)row {
	[typeInfo removeObjectAtIndex: row];
}

- (void)initformTableRemoveRow: (int)row {
	[initforms removeObjectAtIndex: row];
}

- (void)sortTableRemoveRow: (int)row {
	[sortInfo removeObjectAtIndex: row];
}

- (id) emptyArrayOfSize: (int) sz {
	NSMutableArray *newArray = [[NSMutableArray alloc] initWithCapacity: sz];
	int i;
	[newArray insertObject:@"<new type>" atIndex: 0];
	for (i = 1; i < sz; i++) {
		[newArray insertObject:@"" atIndex: i];
	}
	return newArray;
}

- (id) typeTableCellAtRow: (int)row
					  col: (int)col {
	NSMutableArray *rowArray = [typeInfo objectAtIndex:row];
	return [rowArray objectAtIndex:col];
}

- (id) initformTableCellAtRow: (int)row
						  col: (int)col {
	NSMutableArray *rowArray = [initforms objectAtIndex:row];
	return [rowArray objectAtIndex:col];
}

- (id) sortTableCellAtRow: (int)row
					  col: (int)col {
	NSMutableArray *rowArray = [sortInfo objectAtIndex:row];
	return [rowArray objectAtIndex:col];
}

- (void) setValue: (id)newVal 
	forTypeTableCellAtRow: (int)rowIndex
			  col: (int)colIndex {
	NSMutableArray *rowArray = [typeInfo objectAtIndex:rowIndex];
	[rowArray replaceObjectAtIndex:	colIndex withObject: newVal];
}

- (void) setValue: (id)newVal 
forInitformTableCellAtRow: (int)rowIndex
			  col: (int)colIndex {
	NSMutableArray *rowArray = [initforms objectAtIndex:rowIndex];
	[rowArray replaceObjectAtIndex:	colIndex withObject: newVal];
}

- (void) setValue: (id)newVal 
forSortTableCellAtRow: (int)rowIndex
			  col: (int)colIndex {
	NSMutableArray *rowArray = [sortInfo objectAtIndex:rowIndex];
	[rowArray replaceObjectAtIndex:	colIndex withObject: newVal];
}

- (IBAction)insert: (id)sender {
	// don't need to do anything here; implemented in Lisp
}

- (IBAction)addChild: (id)sender {
	// don't need to do anything here; implemented in Lisp
}

- (IBAction)remove: (id)sender {
	// don't need to do anything here; implemented in Lisp
}

@end
