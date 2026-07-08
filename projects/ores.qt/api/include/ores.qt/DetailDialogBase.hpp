/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_DETAIL_DIALOG_BASE_HPP
#define ORES_QT_DETAIL_DIALOG_BASE_HPP

#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/export.hpp"
#include <QIcon>
#include <QWidget>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

class QTabWidget;
class QPushButton;
class QLayout;
class QLineEdit;
class QAction;

namespace ores::qt {

class BadgeCache;
class ChangeReasonCache;
class ImageCache;
class ProvenanceWidget;

/**
 * @brief Base class for all detail dialogs.
 *
 * Provides common functionality for detail dialogs including:
 * - closeRequested signal for decoupled window closing
 * - statusMessage signal for status bar updates
 * - requestClose() method to emit the closeRequested signal
 * - notifySaveSuccess() helper for consistent post-save behavior
 * - Shared tab / provenance management via pure-virtual accessors
 *
 * ## Tab / Provenance Pattern
 *
 * Every derived dialog exposes its QTabWidget and provenance tab via three
 * pure-virtual one-liners, then calls the shared helpers from its
 * setCreateMode() and updateUiFrom*() implementations:
 *
 * @code
 * // In derived .hpp:
 * QTabWidget*       tabWidget()        const override { return ui_->tabWidget; }
 * QWidget*          provenanceTab()    const override { return ui_->provenanceTab; }
 * ProvenanceWidget* provenanceWidget() const override { return ui_->provenanceWidget; }
 *
 * // In setCreateMode():
 * setProvenanceEnabled(!createMode);
 *
 * // In updateUiFromEntity():
 * populateProvenance(entity_.version, entity_.modified_by, entity_.performed_by,
 *                    entity_.recorded_at, entity_.change_reason_code,
 *                    entity_.change_commentary);
 * @endcode
 *
 * ## Save Success Pattern
 *
 * After a successful save operation, dialogs should:
 * 1. Emit their entity-specific "saved" signal (e.g., dimensionSaved)
 * 2. Call notifySaveSuccess() with a status message
 */
class ORES_QT_API DetailDialogBase : public QWidget {
    Q_OBJECT

public:
    using QWidget::QWidget;
    ~DetailDialogBase() override;

    /** Returns true if the dialog has unsaved changes. Subclasses override. */
    virtual bool hasUnsavedChanges() const {
        return false;
    }

    /** Called when server-side data has changed; override to show stale state. */
    virtual void markAsStale() {}

    /**
     * @brief Returns the entity identifier displayed in this dialog;
     * used by controllers to filter notifications.
     */
    [[nodiscard]] virtual QString code() const {
        return {};
    }

    /**
     * @brief Returns true if the user has already confirmed closing this dialog.
     *
     * Set by onCloseClicked() after the user acknowledges unsaved changes.
     * Used by DetachableMdiSubWindow::closeEvent() to avoid showing a second
     * confirmation when the close was initiated via the dialog's Close button.
     */
    bool isCloseConfirmed() const {
        return closeConfirmed_;
    }

    /**
     * @brief Inject the shared change reason cache.
     *
     * Controllers call this immediately after constructing any detail dialog.
     * Once set, derived classes can call promptChangeReason() without passing
     * the cache explicitly.
     */
    void setChangeReasonCache(ChangeReasonCache* cache) {
        changeReasonCache_ = cache;
    }

    /**
     * @brief Inject the shared badge cache.
     *
     * Controllers call this immediately after constructing a detail dialog
     * that renders badges (via BadgeLabelUtils). May be left unset; badge
     * rendering then uses the fallback colours.
     */
    void setBadgeCache(BadgeCache* cache) {
        badgeCache_ = cache;
    }

    /**
     * @brief Inject the shared image cache for the flag/icon editor.
     *
     * Controllers call this on detail dialogs whose entity carries an image_id.
     * Connects cache-load signals to refresh the flag button. Dialogs without a
     * flag never call this and never call initFlagButton(), so there is no flag
     * UI. See initFlagButton() / entityImageId() for the derived-class contract.
     */
    void setImageCache(ImageCache* cache);

signals:
    /**
     * @brief Emitted when the dialog wants to close its container window.
     *
     * Controllers should connect this signal to the container window's
     * close() slot to enable decoupled window management.
     */
    void closeRequested();

    /**
     * @brief Emitted to show a status message in the status bar.
     *
     * Controllers should connect this signal to the main window's status bar.
     */
    void statusMessage(const QString& message);

    /**
     * @brief Emitted when an error occurs that should be shown to the user.
     */
    void errorMessage(const QString& message);

    /**
     * @brief Emitted when the user picks a different flag image.
     *
     * Derived dialogs connect this to their field-change handler so a flag
     * edit marks the dialog dirty like any other field edit.
     */
    void flagEdited();

protected slots:
    /**
     * @brief Open the flag selector and stage the chosen image as pending.
     */
    void onSelectFlagClicked();

    /**
     * @brief Called when the Close button is clicked.
     *
     * Warns the user if there are unsaved changes, then calls requestClose().
     */
    void onCloseClicked();

protected:
    /**
     * @brief Request closure of the container window.
     */
    void requestClose() {
        emit closeRequested();
    }

    /**
     * @brief Notify that a save operation completed successfully.
     */
    void notifySaveSuccess(const QString& message) {
        emit statusMessage(message);
        requestClose();
    }

    // -------------------------------------------------------------------------
    // Pure-virtual accessors — each derived dialog provides one-liner overrides
    // that return the corresponding widget from its own ui_ object.
    // -------------------------------------------------------------------------

    /** @brief Returns the dialog's QTabWidget (named "tabWidget" in .ui). */
    virtual QTabWidget* tabWidget() const = 0;

    /** @brief Returns the Provenance tab widget (named "provenanceTab" in .ui). */
    virtual QWidget* provenanceTab() const = 0;

    /** @brief Returns the promoted ProvenanceWidget (named "provenanceWidget" in .ui). */
    virtual ProvenanceWidget* provenanceWidget() const = 0;

    // -------------------------------------------------------------------------
    // Shared helpers — call from setCreateMode() and updateUiFrom*()
    // -------------------------------------------------------------------------

    /**
     * @brief Return std::optional(v) when v is non-zero, std::nullopt otherwise.
     *
     * Use when reading a spin box value that maps to a nullable database column
     * where zero means "not set".
     */
    template <typename T>
    static std::optional<T> nulloptIfZero(T v) {
        return v != T{} ? std::optional<T>(v) : std::nullopt;
    }

    /**
     * @brief Enable or disable the Provenance tab.
     *
     * Call with @c false in create mode (no provenance data exists yet) and
     * with @c true after loading an existing record.
     */
    void setProvenanceEnabled(bool enabled);

    /**
     * @brief Populate the embedded ProvenanceWidget with entity data.
     *
     * @param recorded_at  Pass a default-constructed time_point to leave the
     *                     Recorded At field blank for entities that do not
     *                     track this column.
     */
    void populateProvenance(int version,
                            const std::string& modified_by,
                            const std::string& performed_by,
                            std::chrono::system_clock::time_point recorded_at,
                            const std::string& change_reason_code,
                            const std::string& change_commentary);

    /** @brief Clear all fields in the embedded ProvenanceWidget. */
    void clearProvenance();

    // -------------------------------------------------------------------------
    // Change reason prompt — centralised for all operation types
    // -------------------------------------------------------------------------

    /**
     * @brief Result of a successful change reason prompt.
     */
    struct change_reason_selection {
        std::string reason_code;
        std::string commentary;
    };

    /**
     * @brief Show the change reason dialog and return the user's selection.
     *
     * Handles all three operation types (Create, Amend, Delete) uniformly.
     * Uses the cache injected via setChangeReasonCache(). Returns std::nullopt
     * if the cache is not ready, no reasons are available, or the user cancels.
     *
     * Usage in onSaveClicked():
     * @code
     * const auto opType = createMode_
     *     ? ChangeReasonDialog::OperationType::Create
     *     : ChangeReasonDialog::OperationType::Amend;
     * const auto sel = promptChangeReason(opType, hasChanges_,
     *     createMode_ ? "system" : "common");
     * if (!sel) return;
     * entity_.change_reason_code = sel->reason_code;
     * entity_.change_commentary  = sel->commentary;
     * @endcode
     *
     * @param opType    Create, Amend, or Delete.
     * @param isDirty   Whether any fields have been modified (used by Amend
     *                  to enable/disable the non-material-update reason).
     * @param category  Category code to filter reasons (default: "system" for
     *                  Create; use "common" for Amend/Delete).
     */
    std::optional<change_reason_selection>
    promptChangeReason(ChangeReasonDialog::OperationType opType,
                       bool isDirty,
                       std::string_view category = "system");

    // -------------------------------------------------------------------------
    // Flag / icon editor — centralised for all entities with an image_id.
    // -------------------------------------------------------------------------

    /**
     * @brief Create the clickable flag button inside @p container.
     *
     * Call once from a derived dialog's constructor, passing the layout of the
     * flag group box from its .ui (e.g. ui_->flagGroup->layout()). The button
     * opens the flag selector on click. Entities without a flag never call this.
     */
    void initFlagButton(QLayout* container);

    /**
     * @brief Current entity's image_id, or nullopt for new / no-flag entities.
     *
     * Derived dialogs with a flag override this to return their entity's
     * image_id. The base uses it to render the flag and seed the selector.
     */
    virtual std::optional<boost::uuids::uuid> entityImageId() const {
        return std::nullopt;
    }

    /**
     * @brief Key line-edit that shows an inline flag icon, or nullptr.
     *
     * Override to return the natural-key field (e.g. the alpha-2 code edit) so
     * the base paints a small flag inside it that tracks the typed value.
     */
    virtual QLineEdit* keyFlagField() const {
        return nullptr;
    }

    /**
     * @brief Flag icon for a key value, used for the inline key-field flag.
     *
     * Override to call the entity-specific cache accessor
     * (e.g. ImageCache::getCountryFlagIcon). Default returns an empty icon.
     */
    virtual QIcon keyFlagIcon(const std::string& key) const {
        (void)key;
        return {};
    }

    /** @brief The shared image cache, or nullptr when the entity has no flag. */
    [[nodiscard]] ImageCache* imageCache() const {
        return imageCache_;
    }

    /** @brief True when the user staged a different flag this session. */
    [[nodiscard]] bool flagChanged() const {
        return flagChanged_;
    }

    /** @brief The user-staged image_id (valid only when flagChanged()). */
    [[nodiscard]] std::optional<boost::uuids::uuid> selectedImageId() const;

    /** @brief Clear the staged-flag state (call after a successful save). */
    void resetFlagChanged() {
        flagChanged_ = false;
        pendingImageId_.clear();
        updateFlagDisplay();
    }

    /** @brief Refresh the flag button icon from pending / entity image_id. */
    void updateFlagDisplay();

protected:
    [[nodiscard]] BadgeCache* badgeCache() const {
        return badgeCache_;
    }

private:
    bool closeConfirmed_ = false;
    ChangeReasonCache* changeReasonCache_ = nullptr;
    BadgeCache* badgeCache_ = nullptr;
    ImageCache* imageCache_ = nullptr;
    QPushButton* flagButton_ = nullptr;
    QAction* keyFlagAction_ = nullptr;
    QString pendingImageId_;
    bool flagChanged_ = false;
};

}

#endif
