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
#ifndef ORES_QT_SYSTEM_SETTING_CONTROLLER_HPP
#define ORES_QT_SYSTEM_SETTING_CONTROLLER_HPP

#include <QDateTime>
#include "ores.qt/EntityController.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.variability/domain/system_setting.hpp"

namespace ores::qt {

class SystemSettingMdiWindow;
class SystemSettingDetailDialog;

/**
 * @brief Controller for system setting management windows.
 *
 * Manages the lifecycle of system setting list and detail windows,
 * handling creation, editing, and deletion of system settings.
 */
class SystemSettingController final : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.system_setting_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    SystemSettingController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        const QString& username,
        QObject* parent = nullptr);

    ~SystemSettingController() override;

    void showListWindow() override;
    void closeAllWindows() override;
    void reloadListWindow() override;

private slots:
    void onAddNewRequested();
    void onShowDetails(const variability::domain::system_setting& flag);
    void onShowHistory(const QString& name);
    void onSystemSettingSaved(const QString& name);
    void onSystemSettingDeleted(const QString& name);
    void onNotificationReceived(const QString& eventType, const QDateTime& timestamp,
                                const QStringList& entityIds, const QString& tenantId);
    void onOpenSystemSettingVersion(const variability::domain::system_setting& flag,
                                  int versionNumber);
    void onRevertSystemSetting(const variability::domain::system_setting& flag);

private:
    void showDetailWindow(const variability::domain::system_setting& flag,
                          bool createMode = false);
    void showHistoryWindow(const QString& name);
    void refreshListWindow();

private:
    SystemSettingMdiWindow* listWindow_;
    DetachableMdiSubWindow* listMdiSubWindow_;
};

}

#endif
