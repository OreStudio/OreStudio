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

#include <chrono>
#include <string>
#include <QWidget>

class QTabWidget;

namespace ores::qt {

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
class DetailDialogBase : public QWidget {
    Q_OBJECT

public:
    using QWidget::QWidget;
    ~DetailDialogBase() override;

    /** Returns true if the dialog has unsaved changes. Subclasses override. */
    virtual bool hasUnsavedChanges() const { return false; }

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

protected slots:
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
    void requestClose() { emit closeRequested(); }

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
};

}

#endif
