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

#include <QWidget>

namespace ores::qt {

/**
 * @brief Base class for all detail dialogs.
 *
 * Provides common functionality for detail dialogs including:
 * - closeRequested signal for decoupled window closing
 * - statusMessage signal for status bar updates
 * - requestClose() method to emit the closeRequested signal
 * - notifySaveSuccess() helper for consistent post-save behavior
 *
 * This allows dialogs to request closure without knowing about their
 * container (e.g., DetachableMdiSubWindow). The controller wires the
 * closeRequested signal to the container's close slot.
 *
 * ## Save Success Pattern
 *
 * After a successful save operation, dialogs should:
 * 1. Emit their entity-specific "saved" signal (e.g., dimensionSaved)
 * 2. Call notifySaveSuccess() with a status message
 *
 * Example:
 * @code
 * if (result.success) {
 *     emit dimensionSaved(code);
 *     notifySaveSuccess(tr("Dimension '%1' saved").arg(code));
 * }
 * @endcode
 */
class DetailDialogBase : public QWidget {
    Q_OBJECT

public:
    using QWidget::QWidget;
    ~DetailDialogBase() override;

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

protected:
    /**
     * @brief Request closure of the container window.
     *
     * Call this instead of parentWidget()->close() to maintain
     * decoupling between the dialog and its container.
     */
    void requestClose() { emit closeRequested(); }

    /**
     * @brief Notify that a save operation completed successfully.
     *
     * This method handles the common post-save behavior:
     * 1. Emits statusMessage with the provided message
     * 2. Closes the dialog via requestClose()
     *
     * Call this after emitting any entity-specific "saved" signal.
     *
     * @param message Status message to display (e.g., "Currency 'USD' saved")
     */
    void notifySaveSuccess(const QString& message) {
        emit statusMessage(message);
        requestClose();
    }
};

}

#endif
