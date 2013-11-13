//
//  MyUniversalAccessHelper.m
//  Zephyros
//
//  Created by Steven Degutis on 3/1/13.
//  Copyright (c) 2013 Steven Degutis. All rights reserved.
//

#import "SDUniversalAccessHelper.h"

@implementation SDUniversalAccessHelper

+ (BOOL) complainIfNeeded {
    Boolean enabled = AXAPIEnabled();
    
    //for 10.8 and older
    NSString *titlePreMavericks = @"Zephyros requires Universal Access";
    NSString *messagePreMavericks =
        @"For Zephyros to function properly, access for assistive devices must "
         "be enabled first.\n\nTo enable this feature, click \"Enable access for "
         "assistive devices\" in the Universal Access pane of System Preferences.";
    NSString *actionButtonTextPreMavericks = @"Open Universal Access";
    NSString *panePreMavericks = @"com.apple.preference.universalaccess";

    //for 10.9 and newer
    NSString *titleMavericksOn = @"Zephyros requires Accessibility control";
    NSString *messageMavericksOn =
        @"For Zephyros to function properly, it must first be allowed to control "
         "your computer.\n\nTo enable this feature, unlock the Privacy tab of "
         "the Security & Privacy pane of System Preferences and check the box "
         "next to Zephyros.";
    NSString *actionButtonTextMavericksOn = @"Open Privacy";
    NSString *paneMavericksOn = @"com.apple.preference.security";

    //same for both
    NSString *dismissButtonText = @"Dismiss";

    //The replacement is the pane to use
    NSString *appleScriptFormat =
      @"tell application \"System Preferences\"\nactivate\nset current pane to pane \"%@\"\nend tell";

    NSString *title, *message, *actionButtonText, *appleScript;

    if([self olderThanMavericks]){
        title = titlePreMavericks;
        message = messagePreMavericks;
        actionButtonText = actionButtonTextPreMavericks;
        appleScript = [NSString stringWithFormat: appleScriptFormat, panePreMavericks];
    } else {
        title = titleMavericksOn;
        message = messageMavericksOn;
        actionButtonText = actionButtonTextMavericksOn;
        appleScript = [NSString stringWithFormat: appleScriptFormat, paneMavericksOn];

    }

    if (!enabled) {
        [NSApp activateIgnoringOtherApps:YES];
        
        NSInteger result = NSRunAlertPanel(title, message, actionButtonText, dismissButtonText, nil);
        
        if (result == NSAlertDefaultReturn) {
            NSAppleScript *a = [[NSAppleScript alloc] initWithSource: appleScript];
            [a executeAndReturnError:nil];
        }
        
        return YES;
    }
    
    return NO;
}

// NO if running newer than OSX 10.8
+ (BOOL) olderThanMavericks {
    SInt32 majorVersion, minorVersion;

    if(Gestalt(gestaltSystemVersionMajor, &majorVersion) == noErr &&
       Gestalt(gestaltSystemVersionMinor, &minorVersion) == noErr)
        if((majorVersion == 10 && minorVersion >= 9 ) || majorVersion > 10)
            return NO;
    return YES;
}

@end
