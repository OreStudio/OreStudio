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
#ifndef ORES_QT_CHANGE_REASON_CONTROLLER_HPP
#define ORES_QT_CHANGE_REASON_CONTROLLER_HPP

#include <QList>
#include <QDateTime>
#include "ores.qt/EntityController.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.iam/domain/change_reason.hpp"
#include "ores.iam/domain/change_reason_category.hpp"

namespace ores::qt {

class ChangeReasonMdiWindow;
class ChangeReasonCache;

/**
 * @brief Controller for change reason management windows.
 *
 * Manages the lifecycle of change reason list and detail windows.
 */
class ChangeReasonController final : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.change_reason_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    ChangeReasonController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        const QString& username,
        ChangeReasonCache* changeReasonCache,
        QList<DetachableMdiSubWindow*>& allDetachableWindows,
        QObject* parent = nullptr);

    ~ChangeReasonController() override;

    void showListWindow() override;
    void closeAllWindows() override;

private slots:
    void onShowDetails(const iam::domain::change_reason& reason);
    void onAddNewRequested();
    void onShowHistory(const QString& code);
    void onNotificationReceived(const QString& eventType, const QDateTime& timestamp,
                                const QStringList& entityIds);
    void onOpenVersion(const iam::domain::change_reason& reason, int versionNumber);
    void onRevertVersion(const iam::domain::change_reason& reason);

private:
    void showDetailWindow(const iam::domain::change_reason& reason);
    void showAddWindow();
    void showHistoryWindow(const QString& code);

private:
    ChangeReasonMdiWindow* listWindow_;
    DetachableMdiSubWindow* listMdiSubWindow_;
    ChangeReasonCache* changeReasonCache_;
    QList<DetachableMdiSubWindow*>& allDetachableWindows_;
};

}

#endif
