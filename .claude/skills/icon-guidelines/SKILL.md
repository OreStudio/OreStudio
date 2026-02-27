---
name: icon-guidelines
description: Visual language reference for ORE Studio icons. Use this skill when selecting icons for UI elements, ensuring consistent iconography across the application.
license: Complete terms in LICENSE.txt
---


# When to use this skill

Use this skill when:

-   Selecting icons for new UI components or features.
-   Reviewing icon usage for consistency across the application.
-   Understanding the semantic meaning of existing icons.
-   Deciding between filled and regular icon variants.


# How to use this skill

1.  **Identify the semantic category** of what you need to represent (action, status, navigation, etc.).
2.  **Find the appropriate icon** in the inventory below.
3.  **Choose the correct variant**: filled for emphasis/active states, regular for passive/default states.
4.  **Follow the usage guidelines** for sizing and context.


# Icon System Overview

ORE Studio uses **Microsoft Fluent UI System Icons**. All icons follow the naming convention:

```
ic_fluent_{concept}_{size}_{variant}.svg
```


## Variants

-   **Regular**: Outline style, used for default/passive states and secondary actions.
-   **Filled**: Solid style, used for active states, emphasis, and primary actions.


## Sizes

-   **16px**: Compact UI, inline with text, dense tables.
-   **20px**: Standard UI elements, buttons, navigation.
-   **32px**: Headers, emphasis areas.
-   **48px**: Large feature icons, onboarding, empty states.


## Usage Principles

1.  **Consistency**: Use the same icon for the same concept throughout the app.
2.  **Clarity over decoration**: If removing an icon loses no meaning, remove it.
3.  **Paired variants**: Use regular for default, filled for hover/active/selected.
4.  **Adequate spacing**: Give icons room to breathe, minimum 4px from text.


# Icon Inventory


## Actions

Icons representing user-initiated operations.


### Add

Create new items, add to lists, insert content.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_add_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_add_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_add_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_add_20_filled.svg)

**Usage**: "New" buttons, add to list, create actions.


### Delete

Remove items, clear content, destructive actions.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_delete_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_delete_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_delete_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_delete_20_filled.svg)

**Usage**: Delete buttons, remove from list. Use filled variant sparingly to indicate destructive nature.


### Delete Dismiss

Purge data, clear all, destructive bulk action with finality.

-   Regular: ![img](../../../projects/ores.qt/resources/icons/ic_fluent_delete_dismiss_20_regular.svg)
-   Filled: ![img](../../../projects/ores.qt/resources/icons/ic_fluent_delete_dismiss_20_filled.svg)

**Usage**: Purge database actions, clear all data, bulk delete operations. Combines delete (trash can) with dismiss (X) to indicate permanent removal with no undo. Always require confirmation before use.


### Edit

Modify existing content, enter edit mode.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_edit_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_edit_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_edit_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_edit_20_filled.svg)

**Usage**: Edit buttons, modify actions, pencil metaphor.


### Save

Persist changes, commit data.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_save_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_save_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_save_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_save_20_filled.svg)

**Usage**: Save buttons, apply changes.


### Checkmark

Confirm action, indicate completion, accept.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_checkmark_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_checkmark_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_checkmark_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_checkmark_20_filled.svg)

**Usage**: Confirmation buttons, success indicators, selected state in lists.


### Dismiss

Close dialogs, cancel operations, clear notifications.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_dismiss_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_dismiss_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_dismiss_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_dismiss_20_filled.svg)

**Usage**: Close buttons, cancel actions, clear/dismiss.


### Star

Favorite, bookmark, rate.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_star_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_star_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_star_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_star_20_filled.svg)

**Usage**: Regular for "not favorited", filled for "favorited" state.


### Wand

Generate synthetic data, auto-fill, magic create.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_wand_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_wand_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_wand_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_wand_20_filled.svg)

**Usage**: Generate buttons, auto-fill actions, create synthetic/test data.


### Record

Start recording, capture session.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_record_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_record_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_record_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_record_20_filled.svg)

**Usage**: Recording controls. Filled indicates actively recording.


### Publish

Deploy data to production, upload, send to target system.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_upload_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_upload_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_upload_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_upload_20_filled.svg)

**Usage**: Publish buttons, deploy actions, upload data to production tables. Represents sending data "upward" to a target system.


### Reload

Refresh data from server, reload content.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_clockwise_16_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_clockwise_16_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_clockwise_16_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_clockwise_16_filled.svg)

**Usage**: Reload buttons, refresh from server, update data. Use 16px size for toolbar buttons. Distinct from Sync (bidirectional) - Reload implies fetching fresh data from source.


### Redo

Redo last undone action, restore previous state.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_clockwise_16_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_clockwise_16_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_clockwise_16_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_clockwise_16_filled.svg)

**Usage**: Redo buttons in edit contexts. Pairs with Undo (arrow\_rotate\_counterclockwise). Both Reload and Redo use the same icon; context determines meaning.


## Navigation

Icons for directional movement and navigation controls.


### Arrow Left

Navigate back, move left.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_left_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_left_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_left_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_left_20_filled.svg)

**Usage**: Back navigation, left scroll controls.


### Arrow Right

Navigate forward, move right.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_right_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_right_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_right_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_right_20_filled.svg)

**Usage**: Forward navigation, right scroll controls, proceed actions.


### Arrow Previous

Step to previous item in sequence.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_previous_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_previous_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_previous_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_previous_20_filled.svg)

**Usage**: Pagination, stepper controls, previous item.


### Arrow Next

Step to next item in sequence.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_next_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_next_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_next_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_next_20_filled.svg)

**Usage**: Pagination, stepper controls, next item.


### Arrow Download

Download content, export data.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_download_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_download_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_download_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_download_20_filled.svg)

**Usage**: Download buttons, export actions.


### Arrow Download CSV

Import CSV data.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_download_csv_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_download_csv_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_download_csv_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_download_csv_20_filled.svg)

**Usage**: Import CSV actions, clearly labeled with text overlay.


### Arrow Download FPML

Import FPML data.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_download_fpml_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_download_fpml_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_download_fpml_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_download_fpml_20_filled.svg)

**Usage**: Import FPML actions, clearly labeled with text overlay.


### Arrow Download ORE

Import ORE data.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_download_ore_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_download_ore_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_download_ore_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_download_ore_20_filled.svg)

**Usage**: Import ORE actions, clearly labeled with text overlay.


### Arrow Upload CSV

Export CSV data.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_upload_csv_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_upload_csv_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_upload_csv_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_upload_csv_20_filled.svg)

**Usage**: Export CSV actions, clearly labeled with text overlay.


### Arrow Upload FPML

Export FPML data.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_upload_fpml_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_upload_fpml_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_upload_fpml_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_upload_fpml_20_filled.svg)

**Usage**: Export FPML actions, clearly labeled with text overlay.


### Arrow Upload ORE

Export ORE data.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_upload_ore_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_upload_ore_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_upload_ore_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_upload_ore_20_filled.svg)

**Usage**: Export ORE actions, clearly labeled with text overlay.


### Arrow Sync

Synchronize data, refresh with server.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_sync_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_sync_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_sync_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_sync_20_filled.svg)

**Usage**: Sync buttons, bidirectional refresh.


### Arrow Clockwise (16px)

Rotate clockwise, redo action.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_clockwise_16_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_clockwise_16_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_clockwise_16_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_clockwise_16_filled.svg)

**Usage**: Redo buttons, clockwise rotation controls.


### Arrow Rotate Counterclockwise

Undo action, rotate counterclockwise.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_arrow_rotate_counterclockwise_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_arrow_rotate_counterclockwise_20_filled.svg)

**Usage**: Undo buttons, counterclockwise rotation, revert changes.


## Status & Feedback

Icons communicating system state and user feedback.


### Info

Informational message, help content, details.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_info_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_info_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_info_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_info_20_filled.svg)

**Usage**: Info tooltips, help buttons, informational banners.


### Warning

Caution, potential issue, requires attention.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_warning_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_warning_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_warning_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_warning_20_filled.svg)

**Usage**: Warning messages, caution indicators. Pair with amber/yellow color.


### Error Circle

Error state, failed operation, critical issue.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_error_circle_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_error_circle_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_error_circle_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_error_circle_20_filled.svg)

**Usage**: Error messages, validation failures, blocked states. Pair with red color.


### Question

Help, unknown state, requires clarification.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_question_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_question_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_question_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_question_20_filled.svg)

**Usage**: Help buttons, FAQ links, contextual help triggers.


## Users & Identity

Icons representing people and account management.


### Person

Single user, current user, profile.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_person_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_person_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_person_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_person_20_filled.svg)

**Usage**: User profile, account menu, single user reference.


### Person Add

Invite user, create account, add team member.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_person_add_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_person_add_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_person_add_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_person_add_20_filled.svg)

**Usage**: Invite buttons, add user actions, registration.


### Person Accounts

Multiple users, team, user management.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_person_accounts_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_person_accounts_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_person_accounts_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_person_accounts_20_filled.svg)

**Usage**: Team views, user management, multi-user contexts.


## Security & Access

Icons for authentication and authorization concepts.


### Lock Closed

Secured, protected, authentication required.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_lock_closed_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_lock_closed_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_lock_closed_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_lock_closed_20_filled.svg)

**Usage**: Locked content, secure areas, login required indicators.


### Lock Open

Unlocked, accessible, public.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_lock_open_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_lock_open_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_lock_open_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_lock_open_20_filled.svg)

**Usage**: Unlocked state, public content, access granted.


### Password Reset (48px)

Reset credentials, forgot password flow.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_password_reset_48_regular.svgprojects/ores.qt/resources/icons/ic_fluent_password_reset_48_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_password_reset_48_filled.svgprojects/ores.qt/resources/icons/ic_fluent_password_reset_48_filled.svg)

**Usage**: Password reset screens, credential recovery flows. Large size for feature prominence.


### Key Multiple

Change master password, manage encryption keys.

-   Regular: ![img](../../../projects/ores.qt/resources/icons/ic_fluent_key_multiple_20_regular.svg)
-   Filled: ![img](../../../projects/ores.qt/resources/icons/ic_fluent_key_multiple_20_filled.svg)

**Usage**: Change master password actions, encryption key management, password vault operations.


## Connection Status

Icons indicating connectivity and network state.


### Plug Connected

Connected to server, online, link established.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_plug_connected_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_plug_connected_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_plug_connected_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_plug_connected_20_filled.svg)

**Usage**: Connection status indicators showing active connection.


### Plug Disconnected

Disconnected, offline, no connection.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_plug_disconnected_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_plug_disconnected_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_plug_disconnected_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_plug_disconnected_20_filled.svg)

**Usage**: Disconnected state, offline mode, connection lost.


### Plug Connected Checkmark

Connection verified, authenticated connection.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_plug_connected_checkmark_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_plug_connected_checkmark_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_plug_connected_checkmark_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_plug_connected_checkmark_20_filled.svg)

**Usage**: Verified connection, authenticated session, secure link confirmed.


### Server Link

Server connection bookmark, saved connection, connection browser.

-   Regular: ![img](../../../projects/ores.qt/resources/icons/ic_fluent_server_link_20_regular.svg)
-   Filled: ![img](../../../projects/ores.qt/resources/icons/ic_fluent_server_link_20_filled.svg)

**Usage**: Connection browser window, saved server connections, bookmark management. Opens the Connection Browser MDI window for managing server connection bookmarks.


## Files & Documents

Icons for file system and document operations.


### Folder

Directory, container, file group.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_folder_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_folder_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_folder_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_folder_20_filled.svg)

**Usage**: Folder navigation, directory listings.


### Folder Open

Open folder, expanded directory, selected container.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_folder_open_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_folder_open_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_folder_open_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_folder_open_20_filled.svg)

**Usage**: Expanded folder state in tree views, selected folder.


### Document Table

Tabular data, spreadsheet, structured document.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_document_table_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_document_table_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_document_table_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_document_table_20_filled.svg)

**Usage**: Table views, data documents, spreadsheet-like content.


### Document Code (16px)

Source code, script, technical document.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_document_code_16_regular.svgprojects/ores.qt/resources/icons/ic_fluent_document_code_16_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_document_code_16_filled.svgprojects/ores.qt/resources/icons/ic_fluent_document_code_16_filled.svg)

**Usage**: Code files, scripts, configuration files.


## Data & Finance

Icons for numerical data and financial concepts.


### Database

Data storage, datasets, data dimensions.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_database_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_database_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_database_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_database_20_filled.svg)

**Usage**: Dataset views, data dimensions (Origin, Nature, Treatment), data assets.


### Code

Coding schemes, classification systems, identifier schemes.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_code_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_code_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_code_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_code_20_filled.svg)

**Usage**: Coding scheme management (LEI, ISIN, etc.), classification systems.


### Book

Methodologies, documentation, reference materials.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_book_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_book_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_book_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_book_20_filled.svg)

**Usage**: Methodology views, data processing documentation, reference guides.


### Clipboard Number 123 (16px)

Numeric data, ID reference, sequence.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_clipboard_number_123_16_regular.svgprojects/ores.qt/resources/icons/ic_fluent_clipboard_number_123_16_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_clipboard_number_123_16_filled.svgprojects/ores.qt/resources/icons/ic_fluent_clipboard_number_123_16_filled.svg)

**Usage**: ID fields, numeric references, serial numbers.


### Currency Dollar Euro

Financial data, multi-currency, monetary values.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_currency_dollar_euro_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_currency_dollar_euro_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_currency_dollar_euro_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_currency_dollar_euro_20_filled.svg)

**Usage**: Currency fields, financial data, monetary transactions.


## Time & History

Icons for temporal concepts.


### Clock (16px)

Time, schedule, duration.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_clock_16_regular.svgprojects/ores.qt/resources/icons/ic_fluent_clock_16_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_clock_16_filled.svgprojects/ores.qt/resources/icons/ic_fluent_clock_16_filled.svg)

**Usage**: Time fields, timestamps, schedule indicators.


### History

Activity history, past actions, audit trail.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_history_20_regular.svgprojects/ores.qt/resources/icons/ic_fluent_history_20_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_history_20_filled.svgprojects/ores.qt/resources/icons/ic_fluent_history_20_filled.svg)

**Usage**: History views, recent items, activity logs.


### Calendar Clock

Scheduler, scheduled jobs, cron tasks.

-   Regular: `projects/ores.qt/resources/icons/fluentui/regular/ic_fluent_calendar_clock_20_regular.svg`
-   Filled: `projects/ores.qt/resources/icons/fluentui/filled/ic_fluent_calendar_clock_20_filled.svg`

**Usage**: Scheduler menu entries, job definition list window title, any UI element representing scheduled/recurring work. Solar fallback: `calendar.svg`.


### Calendar Add

Schedule a new job, create a scheduled task.

-   Regular: `projects/ores.qt/resources/icons/fluentui/regular/ic_fluent_calendar_add_20_regular.svg`
-   Filled: `projects/ores.qt/resources/icons/fluentui/filled/ic_fluent_calendar_add_20_filled.svg`

**Usage**: "Schedule Job" button in job definition list toolbar. Use instead of generic `Add` when the action specifically creates a scheduled task. Solar fallback: `calendar-add.svg`.


### Calendar Cancel

Unschedule a job, deactivate a scheduled task.

-   Regular: `projects/ores.qt/resources/icons/fluentui/regular/ic_fluent_calendar_cancel_20_regular.svg`
-   Filled: `projects/ores.qt/resources/icons/fluentui/filled/ic_fluent_calendar_cancel_20_filled.svg`

**Usage**: "Unschedule" button in job definition list toolbar and detail dialog. Semantically distinct from `Delete` (which implies permanent removal) — unschedule removes from pg_cron while retaining the definition. Solar fallback: `alarm-remove.svg`.


## Text & Formatting

Icons for text manipulation and formatting.


### Text ABC Underline Double (32px)

Text validation, spell check, language processing.

-   Regular: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_text_abc_underline_double_32_regular.svgprojects/ores.qt/resources/icons/ic_fluent_text_abc_underline_double_32_regular.svg)
-   Filled: [~/Development/OreStudio/OreStudio.local1/](file:projects/ores.qt/resources/icons/ic_fluent_text_abc_underline_double_32_filled.svgprojects/ores.qt/resources/icons/ic_fluent_text_abc_underline_double_32_filled.svg)

**Usage**: Spell check, text validation, language tools.


# Quick Reference Table

| Concept             | Icon Name                           | Regular | Filled |
|------------------- |----------------------------------- |------- |------ |
| Add/Create          | add\_20                             | Default | Active |
| Delete/Remove       | delete\_20                          | Default | Danger |
| Purge/Clear All     | delete\_dismiss\_20                 | Default | Danger |
| Edit/Modify         | edit\_20                            | Default | Active |
| Save                | save\_20                            | Default | Active |
| Confirm/Check       | checkmark\_20                       | Default | Done   |
| Cancel/Close        | dismiss\_20                         | Default | Active |
| Favorite            | star\_20                            | Off     | On     |
| Generate            | wand\_20                            | Default | Active |
| Reload              | arrow\_clockwise\_16                | Default | Active |
| Redo                | arrow\_clockwise\_16                | Default | Active |
| Back                | arrow\_left\_20                     | Default | Active |
| Forward             | arrow\_right\_20                    | Default | Active |
| Download            | arrow\_download\_20                 | Default | Active |
| Sync                | arrow\_sync\_20                     | Default | Active |
| Undo                | arrow\_rotate\_counterclockwise\_20 | Default | Active |
| Info                | info\_20                            | Default | Active |
| Warning             | warning\_20                         | Default | Alert  |
| Error               | error\_circle\_20                   | Default | Alert  |
| Help                | question\_20                        | Default | Active |
| User                | person\_20                          | Default | Active |
| Add User            | person\_add\_20                     | Default | Active |
| Team                | person\_accounts\_20                | Default | Active |
| Locked              | lock\_closed\_20                    | Default | Active |
| Unlocked            | lock\_open\_20                      | Default | Active |
| Change Password     | key\_multiple\_20                   | Default | Active |
| Connected           | plug\_connected\_20                 | Default | Active |
| Disconnected        | plug\_disconnected\_20              | Default | Alert  |
| Verified Connection | plug\_connected\_checkmark\_20      | Default | Active |
| Server Link         | server\_link\_20                    | Default | Active |
| Folder              | folder\_20                          | Closed  | Active |
| Folder Open         | folder\_open\_20                    | Open    | Active |
| Table Document      | document\_table\_20                 | Default | Active |
| Code Document       | document\_code\_16                  | Default | Active |
| Code/Scheme         | code\_20                            | Default | Active |
| Database/Dataset    | database\_20                        | Default | Active |
| Book/Methodology    | book\_20                            | Default | Active |
| Currency            | currency\_dollar\_euro\_20          | Default | Active |
| Time                | clock\_16                           | Default | Active |
| History             | history\_20                         | Default | Active |
| Scheduler/Jobs      | calendar\_clock\_20                 | Default | Active |
| Schedule Job        | calendar\_add\_20                   | Default | Active |
| Unschedule Job      | calendar\_cancel\_20                | Default | Active |
| Import CSV          | arrow\_download\_csv\_20            | Default | Active |
| Import FPML         | arrow\_download\_fpml\_20           | Default | Active |
| Import ORE          | arrow\_download\_ore\_20            | Default | Active |
| Export CSV          | arrow\_upload\_csv\_20              | Default | Active |
| Export FPML         | arrow\_upload\_fpml\_20             | Default | Active |
| Export ORE          | arrow\_upload\_ore\_20              | Default | Active |
| Publish             | arrow\_upload\_20                   | Default | Active |


# Adding New Icons

When adding a new icon to ORE Studio, follow these steps:


## Step 1: Locate Icons in Source Repositories

ORE Studio supports two icon sets:

-   **Fluent UI System Icons**: ~/Development/fluentui-system-icons-main/assets/
-   **Solar Icons**: ~/Development/solar-icons/packages/core/svgs/

Search for a semantically appropriate icon in both sets. For Fluent UI, look for 20px variants (regular and filled). For Solar, look for Linear and Bold variants.

Example search:

```
find ~/Development/fluentui-system-icons-main/assets -name "*upload*20*"
find ~/Development/solar-icons/packages/core/svgs -name "*upload*"
```


## Step 2: Copy Icons to Project

Copy the icons to the correct locations:

```
# Fluent UI icons (regular and filled variants)
cp ".../Arrow Upload/SVG/ic_fluent_arrow_upload_20_regular.svg" \
   projects/ores.qt/resources/icons/

cp ".../Arrow Upload/SVG/ic_fluent_arrow_upload_20_filled.svg" \
   projects/ores.qt/resources/icons/

# Solar icons (Linear and Bold variants)
cp ".../arrows-action/Linear/upload-square.svg" \
   projects/ores.qt/resources/icons/solarized/Linear/

cp ".../arrows-action/Bold/upload-square.svg" \
   projects/ores.qt/resources/icons/solarized/Bold/
```


## Step 3: Update Resource File

Add the new icons to `projects/ores.qt/resources/resources.qrc`:

```xml
<!-- Fluent UI icons -->
<file>icons/ic_fluent_arrow_upload_20_regular.svg</file>
<file>icons/ic_fluent_arrow_upload_20_filled.svg</file>

<!-- Solar icons -->
<file>icons/solarized/Linear/upload-square.svg</file>
<file>icons/solarized/Bold/upload-square.svg</file>
```


## Step 4: Add Enum Entry

Add the new icon to the `Icon` enum in `IconUtils.hpp`:

```cpp
enum class Icon {
    // ... existing entries (alphabetically sorted)
    PlugDisconnected,
    Publish,       // <- New entry
    Question,
    // ...
};
```


## Step 5: Add Icon Mapping

Add the mapping in `IconUtils.cpp` in the `getIconDef()` function:

```cpp
case Icon::Publish: return {"ic_fluent_arrow_upload_20", "upload-square.svg"};
```

The mapping format is:

-   First string: Fluent UI icon base name (without `_regular.svg` / `_filled.svg`)
-   Second string: Solar icon filename
-   Optional third parameter: `true` to force filled/bold variant


## Step 6: Use the Icon

Use the new icon in code:

```cpp
action->setIcon(IconUtils::createRecoloredIcon(
    Icon::Publish, IconUtils::DefaultIconColor));
```


## Step 7: Document the Icon

Update this skill document:

1.  Add the icon to the appropriate category in the Icon Inventory section
2.  Add an entry to the Quick Reference Table
