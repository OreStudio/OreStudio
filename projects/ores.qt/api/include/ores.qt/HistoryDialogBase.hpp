/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_HISTORY_DIALOG_BASE_HPP
#define ORES_QT_HISTORY_DIALOG_BASE_HPP

#include <QString>
#include <QWidget>
#include "ores.qt/export.hpp"

namespace ores::qt {

/**
 * @brief Base class for all history dialogs.
 *
 * Provides:
 * - statusChanged / errorOccurred signals shared by all history dialogs.
 * - markAsStale() — called by the controller when a server-side change event
 *   arrives; default is a no-op; derived classes override to show a stale
 *   indicator and/or reload.
 * - code() — returns the entity identifier displayed in this dialog;
 *   used by controllers to filter notifications.
 */
class ORES_QT_API HistoryDialogBase : public QWidget {
    Q_OBJECT

public:
    using QWidget::QWidget;
    ~HistoryDialogBase() override;

    virtual void markAsStale() {}
    [[nodiscard]] virtual QString code() const { return {}; }

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
};

}

#endif
