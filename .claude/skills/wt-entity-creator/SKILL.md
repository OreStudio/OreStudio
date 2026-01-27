---
name: wt-entity-creator
description: Create Wt web UI components for domain entities including list widgets, detail dialogs, and application integration.
license: Complete terms in LICENSE.txt
---


# When to use this skill

When you need to add a new entity to the Wt web UI layer in ORE Studio. This skill guides you through creating all the necessary Wt components to display, edit, and manage a domain entity, following established patterns in the codebase.

Prerequisites:

-   The domain type must already exist (see [domain-type-creator](../domain-type-creator/SKILL.md) skill)
-   The service layer for the entity must exist in the relevant domain component (e.g., `ores.refdata`, `ores.iam`)


# How to use this skill

1.  Gather entity requirements (name, fields, features needed).
2.  Follow the detailed instructions to create components in order.
3.  Each phase ends with a PR checkpoint - raise PR, wait for review, merge.
4.  Create a fresh branch from main for the next phase (see [feature-branch-manager](../feature-branch-manager/SKILL.md)).
5.  Build and test after each step within a phase.


# PR Strategy

This skill is structured into **three phases**, each resulting in a separate PR. This keeps PRs reviewable and allows incremental integration.

| Phase | Steps     | PR Title Template                          |
|----- |--------- |------------------------------------------ |
| 1     | Steps 1-2 | `[wt] Add <Entity> list widget`            |
| 2     | Steps 3-4 | `[wt] Add <Entity> dialog`                 |
| 3     | Steps 5-6 | `[wt] Integrate <Entity> into application` |

After each PR is merged, use the [feature-branch-manager](../feature-branch-manager/SKILL.md) skill to transition to the next phase. This ensures clean git history by creating fresh branches from main rather than rebasing.


# Detailed instructions

The following sections describe the step-by-step process for creating a complete Wt entity.


## Gather Requirements

Before starting, gather the following information:

-   **Entity name**: The name of the entity (e.g., `currency`, `country`, `account`).
-   **Component location**: Which domain the entity belongs to (e.g., `ores.refdata`, `ores.iam`).
-   **Service class**: The service that provides CRUD operations (e.g., `currency_service`).
-   **Display fields**: Which fields to show in the list view table.
-   **Editable fields**: Which fields can be edited in the detail dialog.
-   **Primary key**: The unique identifier field (e.g., `iso_code`, `id`, `alpha2_code`).
-   **Features needed**:
    -   [ ] List view with table
    -   [ ] Detail dialog (create/edit)
    -   [ ] Delete confirmation
    -   [ ] Field validation


# Phase 1: List Widget

This phase creates the list widget that displays entities in a table. After completing Steps 1-2, raise a PR.

**Suggested PR title:** `[wt] Add <Entity> list widget`


## Step 1: Create Data Structures

Create the data transfer structures for form binding and table display.


### File locations

Add to or create: `projects/ores.wt/include/ores.wt/app/<entity>_list_widget.hpp`


### Row structure for table display

```cpp
struct <entity>_row {
    std::string primary_key;  // e.g., iso_code, alpha2_code
    std::string name;
    // Add display fields
    int version = 0;
};
```

This structure contains only the fields needed for table display, keeping the table lightweight.


### Commit message

```
[wt] Add <entity>_row structure for table display

Create row structure containing display fields for the <entity> list
table.
```


## Step 2: Create List Widget

Create the list widget showing entities in a table with toolbar actions.


### File locations

-   Header: `projects/ores.wt/include/ores.wt/app/<entity>_list_widget.hpp`
-   Implementation: `projects/ores.wt/src/app/<entity>_list_widget.cpp`


### Header structure

```cpp
#ifndef ORES_WT_<ENTITY>_LIST_WIDGET_HPP
#define ORES_WT_<ENTITY>_LIST_WIDGET_HPP

#include <string>
#include <vector>
#include <Wt/WContainerWidget.h>
#include <Wt/WTable.h>
#include <Wt/WText.h>
#include <Wt/WPushButton.h>
#include <Wt/WSignal.h>

namespace ores::wt::app {

struct <entity>_row {
    std::string primary_key;
    std::string name;
    // Add other display fields
    int version = 0;
};

class <entity>_list_widget final : public Wt::WContainerWidget {
public:
    <entity>_list_widget();

    // Data population
    void set_<entity>s(const std::vector<<entity>_row>& entries);
    void refresh();

    // Signals for parent to handle
    Wt::Signal<>& add_requested() { return add_requested_; }
    Wt::Signal<>& refresh_requested() { return refresh_requested_; }
    Wt::Signal<std::string>& edit_requested() { return edit_requested_; }
    Wt::Signal<std::string>& delete_requested() { return delete_requested_; }

private:
    void setup_toolbar();
    void setup_table();
    void populate_table();

    Wt::WTable* table_ = nullptr;
    Wt::WText* status_text_ = nullptr;

    std::vector<<entity>_row> entries_;

    Wt::Signal<> add_requested_;
    Wt::Signal<> refresh_requested_;
    Wt::Signal<std::string> edit_requested_;
    Wt::Signal<std::string> delete_requested_;
};

}

#endif
```


### Implementation patterns

-   Constructor setup sequence

    ```cpp
    <entity>_list_widget::<entity>_list_widget() {
        addStyleClass("container-fluid p-3");
        setup_toolbar();
        setup_table();
    }
    ```

-   Toolbar setup

    ```cpp
    void <entity>_list_widget::setup_toolbar() {
        auto* toolbar = addWidget(std::make_unique<Wt::WContainerWidget>());
        toolbar->addStyleClass("d-flex gap-2 mb-3 align-items-center");
    
        // Add button
        auto* add_btn = toolbar->addWidget(
            std::make_unique<Wt::WPushButton>("Add <Entity>"));
        add_btn->addStyleClass("btn btn-primary");
        add_btn->clicked().connect([this] { add_requested_.emit(); });
    
        // Refresh button
        auto* refresh_btn = toolbar->addWidget(
            std::make_unique<Wt::WPushButton>("Refresh"));
        refresh_btn->addStyleClass("btn btn-secondary");
        refresh_btn->clicked().connect([this] {
            refresh();  // Updates status text to "Refreshing..."
            refresh_requested_.emit();  // Signal parent to reload data
        });
    
        // Status text
        status_text_ = toolbar->addWidget(std::make_unique<Wt::WText>());
        status_text_->addStyleClass("text-muted ms-auto");
    }
    ```

-   Table setup

    ```cpp
    void <entity>_list_widget::setup_table() {
        table_ = addWidget(std::make_unique<Wt::WTable>());
        table_->addStyleClass("table table-striped table-hover");
        table_->setHeaderCount(1);
    
        // Header row
        int col = 0;
        table_->elementAt(0, col++)->addWidget(
            std::make_unique<Wt::WText>("Primary Key"));
        table_->elementAt(0, col++)->addWidget(
            std::make_unique<Wt::WText>("Name"));
        // Add other column headers
        table_->elementAt(0, col++)->addWidget(
            std::make_unique<Wt::WText>("Version"));
        table_->elementAt(0, col++)->addWidget(
            std::make_unique<Wt::WText>("Actions"));
    
        // Make header sticky
        for (int i = 0; i < col; ++i) {
            table_->elementAt(0, i)->addStyleClass("sticky-top bg-light");
        }
    }
    ```

-   Table population

    ```cpp
    void <entity>_list_widget::populate_table() {
        // Clear existing rows (keep header)
        while (table_->rowCount() > 1) {
            table_->deleteRow(1);
        }
    
        for (const auto& entry : entries_) {
            int row = table_->rowCount();
            int col = 0;
    
            table_->elementAt(row, col++)->addWidget(
                std::make_unique<Wt::WText>(entry.primary_key));
            table_->elementAt(row, col++)->addWidget(
                std::make_unique<Wt::WText>(entry.name));
            // Add other fields
            table_->elementAt(row, col++)->addWidget(
                std::make_unique<Wt::WText>(std::to_string(entry.version)));
    
            // Actions cell
            auto* actions = table_->elementAt(row, col++);
            actions->addStyleClass("d-flex gap-1");
    
            auto* edit_btn = actions->addWidget(
                std::make_unique<Wt::WPushButton>("Edit"));
            edit_btn->addStyleClass("btn btn-sm btn-outline-primary");
            edit_btn->clicked().connect([this, pk = entry.primary_key] {
                edit_requested_.emit(pk);
            });
    
            auto* delete_btn = actions->addWidget(
                std::make_unique<Wt::WPushButton>("Delete"));
            delete_btn->addStyleClass("btn btn-sm btn-outline-danger");
            delete_btn->clicked().connect([this, pk = entry.primary_key] {
                delete_requested_.emit(pk);
            });
        }
    
        status_text_->setText(std::to_string(entries_.size()) + " <entity>(s)");
    }
    
    void <entity>_list_widget::set_<entity>s(
        const std::vector<<entity>_row>& entries) {
        entries_ = entries;
        populate_table();
    }
    
    void <entity>_list_widget::refresh() {
        status_text_->setText("Refreshing...");
    }
    ```


### CMakeLists.txt update

Add the new source file to `projects/ores.wt/CMakeLists.txt`:

```cmake
set(ores_wt_sources
    # ... existing sources
    src/app/<entity>_list_widget.cpp
)
```


### Commit message

```
[wt] Add <entity>_list_widget for <entity> list view

Create list widget with table, toolbar actions (add, refresh), and
row actions (edit, delete).
```


## Phase 1 Checkpoint: Raise PR

At this point:

1.  Build and verify: `cmake --build --preset linux-clang-debug`
2.  Verify the widget compiles correctly.
3.  Commit all changes.
4.  Push branch and raise PR.

**PR Title:** `[wt] Add <Entity> list widget`

**PR Description:**

```
## Summary

- Add <entity>_row structure for table display
- Add <entity>_list_widget with table and toolbar

```

Wait for review feedback and merge before continuing to Phase 2.


# Phase 2: Detail Dialog

After Phase 1 PR is merged, use [feature-branch-manager](../feature-branch-manager/SKILL.md) to transition to Phase 2.

**Suggested PR title:** `[wt] Add <Entity> dialog`


## Step 3: Create Data Structure for Form

Create the data structure for form binding.


### File location

Add to: `projects/ores.wt/include/ores.wt/app/<entity>_dialog.hpp`


### Data structure for form binding

```cpp
struct <entity>_data {
    std::string primary_key;
    std::string name;
    // Add all editable fields with appropriate types
    // Use std::string for text, int for numbers, bool for checkboxes
    int version = 0;
};
```

This structure mirrors all editable fields in the entity, used for form data binding.


### Commit message

```
[wt] Add <entity>_data structure for form binding

Create data structure containing all editable fields for the <entity>
dialog form.
```


## Step 4: Create Detail Dialog

Create the dialog for creating and editing entities.


### File locations

-   Header: `projects/ores.wt/include/ores.wt/app/<entity>_dialog.hpp`
-   Implementation: `projects/ores.wt/src/app/<entity>_dialog.cpp`


### Header structure

```cpp
#ifndef ORES_WT_<ENTITY>_DIALOG_HPP
#define ORES_WT_<ENTITY>_DIALOG_HPP

#include <string>
#include <Wt/WDialog.h>
#include <Wt/WLineEdit.h>
#include <Wt/WSpinBox.h>
#include <Wt/WComboBox.h>
#include <Wt/WText.h>
#include <Wt/WSignal.h>

namespace ores::wt::app {

struct <entity>_data {
    std::string primary_key;
    std::string name;
    // Add editable fields
    int version = 0;
};

class <entity>_dialog final : public Wt::WDialog {
public:
    enum class mode { add, edit };

    explicit <entity>_dialog(mode m);

    void set_<entity>(const <entity>_data& data);
    <entity>_data get_<entity>() const;

    Wt::Signal<>& saved() { return saved_; }

private:
    void setup_form();
    void setup_buttons();
    bool validate();

    mode mode_;

    // Form fields
    Wt::WLineEdit* primary_key_edit_ = nullptr;
    Wt::WLineEdit* name_edit_ = nullptr;
    // Add other field widgets
    Wt::WText* status_text_ = nullptr;

    Wt::Signal<> saved_;
};

}

#endif
```


### Implementation patterns

-   Constructor

    ```cpp
    <entity>_dialog::<entity>_dialog(mode m)
        : Wt::WDialog(m == mode::add ? "Add <Entity>" : "Edit <Entity>"),
          mode_(m) {
        setModal(true);
        setResizable(true);
        setClosable(true);
        setWidth(500);
    
        setup_form();
        setup_buttons();
    }
    ```

-   Form setup

    ```cpp
    void <entity>_dialog::setup_form() {
        auto* content = contents();
        content->addStyleClass("p-3");
    
        auto* form = content->addWidget(std::make_unique<Wt::WContainerWidget>());
    
        // Primary key field (read-only in edit mode)
        auto* pk_group = form->addWidget(std::make_unique<Wt::WContainerWidget>());
        pk_group->addStyleClass("mb-3");
        pk_group->addWidget(std::make_unique<Wt::WText>("Primary Key"))
            ->addStyleClass("form-label");
        primary_key_edit_ = pk_group->addWidget(std::make_unique<Wt::WLineEdit>());
        primary_key_edit_->addStyleClass("form-control");
        primary_key_edit_->setMaxLength(32);  // Adjust as needed
        if (mode_ == mode::edit) {
            primary_key_edit_->setReadOnly(true);
            primary_key_edit_->addStyleClass("bg-light");
        }
    
        // Name field
        auto* name_group = form->addWidget(std::make_unique<Wt::WContainerWidget>());
        name_group->addStyleClass("mb-3");
        name_group->addWidget(std::make_unique<Wt::WText>("Name"))
            ->addStyleClass("form-label");
        name_edit_ = name_group->addWidget(std::make_unique<Wt::WLineEdit>());
        name_edit_->addStyleClass("form-control");
    
        // Add other fields following same pattern
        // For spinbox: std::make_unique<Wt::WSpinBox>()
        // For combobox: std::make_unique<Wt::WComboBox>()
    
        // Status text for validation errors
        status_text_ = form->addWidget(std::make_unique<Wt::WText>());
        status_text_->addStyleClass("text-danger mt-2");
    }
    ```

-   Buttons setup

    ```cpp
    void <entity>_dialog::setup_buttons() {
        auto* footer = this->footer();
        footer->addStyleClass("d-flex gap-2 justify-content-end");
    
        auto* save_btn = footer->addWidget(
            std::make_unique<Wt::WPushButton>("Save"));
        save_btn->addStyleClass("btn btn-primary");
        save_btn->clicked().connect([this] {
            if (validate()) {
                saved_.emit();
                accept();
            }
        });
    
        auto* cancel_btn = footer->addWidget(
            std::make_unique<Wt::WPushButton>("Cancel"));
        cancel_btn->addStyleClass("btn btn-secondary");
        cancel_btn->clicked().connect([this] { reject(); });
    }
    ```

-   Validation

    ```cpp
    bool <entity>_dialog::validate() {
        status_text_->setText("");
    
        // Primary key validation
        auto pk = primary_key_edit_->text().toUTF8();
        if (pk.empty()) {
            status_text_->setText("Primary key is required");
            return false;
        }
        // Add length/format validation as needed
    
        // Name validation
        auto name = name_edit_->text().toUTF8();
        if (name.empty()) {
            status_text_->setText("Name is required");
            return false;
        }
    
        // Add other validations
    
        return true;
    }
    ```

-   Data getters/setters

    ```cpp
    void <entity>_dialog::set_<entity>(const <entity>_data& data) {
        primary_key_edit_->setText(data.primary_key);
        name_edit_->setText(data.name);
        // Set other fields
    }
    
    <entity>_data <entity>_dialog::get_<entity>() const {
        <entity>_data data;
        data.primary_key = primary_key_edit_->text().toUTF8();
        data.name = name_edit_->text().toUTF8();
        // Get other fields
        return data;
    }
    ```


### CMakeLists.txt update

Add the new source file to `projects/ores.wt/CMakeLists.txt`:

```cmake
set(ores_wt_sources
    # ... existing sources
    src/app/<entity>_dialog.cpp
)
```


### Commit message

```
[wt] Add <entity>_dialog for <entity> create/edit

Create dialog with form fields, validation, and add/edit mode support.
```


## Phase 2 Checkpoint: Raise PR

At this point:

1.  Build and verify: `cmake --build --preset linux-clang-debug`
2.  Verify the dialog compiles correctly.
3.  Commit all changes.
4.  Push branch and raise PR.

**PR Title:** `[wt] Add <Entity> dialog`

**PR Description:**

```
## Summary

- Add <entity>_data structure for form binding
- Add <entity>_dialog with add/edit modes and validation

```

Wait for review feedback and merge before continuing to Phase 3.


# Phase 3: Application Integration

After Phase 2 PR is merged, use [feature-branch-manager](../feature-branch-manager/SKILL.md) to transition to Phase 3.

**Suggested PR title:** `[wt] Integrate <Entity> into application`


## Step 5: Create Conversion Functions

Create functions to convert between domain objects and UI structures.


### File location

Add to: `projects/ores.wt/src/app/ore_application.cpp`


### Conversion functions

```cpp
namespace {

// Domain -> Row (for list display)
<entity>_row to_row(const <component>::domain::<entity>& e) {
    <entity>_row row;
    row.primary_key = e.primary_key();
    row.name = e.name();
    // Map other display fields
    row.version = e.version();
    return row;
}

// Form -> Domain (for saving)
<component>::domain::<entity> to_domain(
    const <entity>_data& d,
    const std::string& username) {
    <component>::domain::<entity> entity;
    entity.set_primary_key(d.primary_key);
    entity.set_name(d.name);
    // Map other editable fields
    entity.set_recorded_by(username);
    entity.set_version(d.version);
    return entity;
}

// Domain -> Form (for editing)
<entity>_data to_data(const <component>::domain::<entity>& e) {
    <entity>_data data;
    data.primary_key = e.primary_key();
    data.name = e.name();
    // Map other editable fields
    data.version = e.version();
    return data;
}

}
```


### Commit message

```
[wt] Add <entity> conversion functions

Add to_row, to_domain, and to_data functions for converting between
domain and UI structures.
```


## Step 6: Integrate into ore\_application

Wire up the entity handlers in the main application.


### Files to modify

-   `projects/ores.wt/include/ores.wt/app/ore_application.hpp`
-   `projects/ores.wt/src/app/ore_application.cpp`


### Header changes

Add to `ore_application.hpp`:

```cpp
class <entity>_list_widget;

class ore_application : public Wt::WApplication {
    // ... existing code

private:
    // Add member
    <entity>_list_widget* <entity>_list_widget_ = nullptr;

    // Add handlers
    void setup_<entity>_handlers();
    void load_<entity>s();
    void show_add_<entity>_dialog();
    void show_edit_<entity>_dialog(const std::string& primary_key);
    void confirm_delete_<entity>(const std::string& primary_key);
};
```


### Implementation patterns

-   Handler setup

    ```cpp
    void ore_application::setup_<entity>_handlers() {
        <entity>_list_widget_->add_requested().connect([this] {
            show_add_<entity>_dialog();
        });
    
        <entity>_list_widget_->refresh_requested().connect([this] {
            load_<entity>s();
        });
    
        <entity>_list_widget_->edit_requested().connect([this](const std::string& pk) {
            show_edit_<entity>_dialog(pk);
        });
    
        <entity>_list_widget_->delete_requested().connect([this](const std::string& pk) {
            confirm_delete_<entity>(pk);
        });
    }
    ```

-   Load entities

    ```cpp
    void ore_application::load_<entity>s() {
        auto& ctx = application_context::instance();
        auto entries = ctx.<entity>_service().list_<entity>s(0, max_<entity>s_to_load);
    
        std::vector<<entity>_row> rows;
        rows.reserve(entries.size());
        for (const auto& e : entries) {
            rows.push_back(to_row(e));
        }
    
        <entity>_list_widget_->set_<entity>s(rows);
    }
    ```

-   Show add dialog

    ```cpp
    void ore_application::show_add_<entity>_dialog() {
        auto dialog = std::make_unique<<entity>_dialog>(<entity>_dialog::mode::add);
        auto* dlg = dialog.get();
    
        dlg->saved().connect([this, dlg] {
            auto data = dlg->get_<entity>();
            auto entity = to_domain(data, username_);
    
            auto& ctx = application_context::instance();
            ctx.<entity>_service().save_<entity>(entity);
    
            load_<entity>s();
        });
    
        // Cleanup dialog when closed
        dlg->finished().connect([this, dlg] {
            removeChild(dlg);
        });
    
        dlg->show();
        addChild(std::move(dialog));
    }
    ```

-   Show edit dialog

    ```cpp
    void ore_application::show_edit_<entity>_dialog(const std::string& primary_key) {
        auto& ctx = application_context::instance();
        auto entity = ctx.<entity>_service().get_<entity>(primary_key);
        if (!entity) {
            // Show error message
            return;
        }
    
        auto dialog = std::make_unique<<entity>_dialog>(<entity>_dialog::mode::edit);
        auto* dlg = dialog.get();
        dlg->set_<entity>(to_data(*entity));
    
        dlg->saved().connect([this, dlg] {
            auto data = dlg->get_<entity>();
            auto entity = to_domain(data, username_);
    
            auto& ctx = application_context::instance();
            ctx.<entity>_service().save_<entity>(entity);
    
            load_<entity>s();
        });
    
        // Cleanup dialog when closed
        dlg->finished().connect([this, dlg] {
            removeChild(dlg);
        });
    
        dlg->show();
        addChild(std::move(dialog));
    }
    ```

-   Delete confirmation

    ```cpp
    void ore_application::confirm_delete_<entity>(const std::string& primary_key) {
        auto* msgbox = addChild(std::make_unique<Wt::WMessageBox>(
            "Confirm Delete",
            "Are you sure you want to delete this <entity>?",
            Wt::Icon::Question,
            Wt::StandardButton::Yes | Wt::StandardButton::No));
    
        msgbox->buttonClicked().connect([this, msgbox, primary_key](
            Wt::StandardButton btn) {
            if (btn == Wt::StandardButton::Yes) {
                auto& ctx = application_context::instance();
                ctx.<entity>_service().delete_<entity>(primary_key);
                load_<entity>s();
            }
            removeChild(msgbox);
        });
    
        msgbox->show();
    }
    ```

-   Main view integration

    In `show_main_view()`, add the widget to a tab or navigation:
    
    ```cpp
    // Create widget
    <entity>_list_widget_ = container->addWidget(
        std::make_unique<<entity>_list_widget>());
    
    // Setup handlers
    setup_<entity>_handlers();
    
    // Load initial data
    load_<entity>s();
    ```


### Commit message

```
[wt] Integrate <entity> into ore_application

Add handlers for add/edit/delete operations, conversion functions,
and widget to main navigation.
```


## Phase 3 Checkpoint: Raise PR

At this point:

1.  Build and verify: `cmake --build --preset linux-clang-debug`
2.  Run full end-to-end test of the entity.
3.  Test add, edit, and delete operations.
4.  Commit all changes.
5.  Push branch and raise PR.

**PR Title:** `[wt] Integrate <Entity> into application`

**PR Description:**

```
## Summary

- Add conversion functions (to_row, to_domain, to_data)
- Integrate <entity>_list_widget into main view
- Add handlers for add/edit/delete operations

```


# Common patterns reference


## Bootstrap 5 CSS classes

Common classes used in ores.wt:

| Purpose              | Classes                                         |
|-------------------- |----------------------------------------------- |
| Container            | `container-fluid p-3`                           |
| Toolbar              | `d-flex gap-2 mb-3 align-items-center`          |
| Primary button       | `btn btn-primary`                               |
| Secondary button     | `btn btn-secondary`                             |
| Small button         | `btn btn-sm`                                    |
| Outline button       | `btn btn-outline-primary`, `btn-outline-danger` |
| Table                | `table table-striped table-hover`               |
| Form control         | `form-control`                                  |
| Form label           | `form-label`                                    |
| Margin bottom        | `mb-3`                                          |
| Text muted           | `text-muted`                                    |
| Error text           | `text-danger`                                   |
| Read-only background | `bg-light`                                      |


## Form field types

| Field Type | Wt Widget        | Example         |
|---------- |---------------- |--------------- |
| Text       | `WLineEdit`      | Name, code      |
| Number     | `WSpinBox`       | Version, count  |
| Decimal    | `WDoubleSpinBox` | Amount, rate    |
| Selection  | `WComboBox`      | Type, category  |
| Boolean    | `WCheckBox`      | Active, enabled |
| Date       | `WDateEdit`      | Start date      |
| DateTime   | `WDateTimeEdit`  | Timestamp       |
| Large text | `WTextArea`      | Description     |


## Signal patterns

```cpp
// Define signal
Wt::Signal<> simple_signal_;
Wt::Signal<std::string> string_signal_;
Wt::Signal<int, std::string> multi_param_signal_;

// Expose signal
Wt::Signal<>& simple_signal() { return simple_signal_; }

// Emit signal
simple_signal_.emit();
string_signal_.emit("value");
multi_param_signal_.emit(42, "text");

// Connect to signal
widget->simple_signal().connect([this] { /* handler */ });
widget->string_signal().connect([this](const std::string& val) { /* handler */ });
```


## Related skills

-   [feature-branch-manager](../feature-branch-manager/SKILL.md) - For transitioning between phases
-   [Domain Type Creator](../domain-type-creator/SKILL.md) - For creating the underlying domain type
-   [Qt Entity Creator](../qt-entity-creator/SKILL.md) - Similar skill for Qt UI layer
