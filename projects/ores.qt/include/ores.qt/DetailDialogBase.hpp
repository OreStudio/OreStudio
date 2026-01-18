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
 * - requestClose() method to emit the signal
 *
 * This allows dialogs to request closure without knowing about their
 * container (e.g., DetachableMdiSubWindow). The controller wires the
 * closeRequested signal to the container's close slot.
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

protected:
    /**
     * @brief Request closure of the container window.
     *
     * Call this instead of parentWidget()->close() to maintain
     * decoupling between the dialog and its container.
     */
    void requestClose() { emit closeRequested(); }
};

}

#endif
