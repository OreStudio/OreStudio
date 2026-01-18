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
#ifndef ORES_QT_FEATURE_FLAG_CONTROLLER_HPP
#define ORES_QT_FEATURE_FLAG_CONTROLLER_HPP

#include <QDateTime>
#include "ores.qt/EntityController.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.variability/domain/feature_flags.hpp"

namespace ores::qt {

class FeatureFlagMdiWindow;
class FeatureFlagDetailDialog;

/**
 * @brief Controller for feature flag management windows.
 *
 * Manages the lifecycle of feature flag list and detail windows,
 * handling creation, editing, and deletion of feature flags.
 */
class FeatureFlagController final : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.feature_flag_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    FeatureFlagController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        const QString& username,
        QObject* parent = nullptr);

    ~FeatureFlagController() override;

    void showListWindow() override;
    void closeAllWindows() override;
    void reloadListWindow() override;

private slots:
    void onAddNewRequested();
    void onShowDetails(const variability::domain::feature_flags& flag);
    void onShowHistory(const QString& name);
    void onFeatureFlagSaved(const QString& name);
    void onFeatureFlagDeleted(const QString& name);
    void onNotificationReceived(const QString& eventType, const QDateTime& timestamp,
                                const QStringList& entityIds);
    void onOpenFeatureFlagVersion(const variability::domain::feature_flags& flag,
                                  int versionNumber);
    void onRevertFeatureFlag(const variability::domain::feature_flags& flag);

private:
    void showDetailWindow(const variability::domain::feature_flags& flag,
                          bool createMode = false);
    void showHistoryWindow(const QString& name);
    void refreshListWindow();

private:
    FeatureFlagMdiWindow* listWindow_;
    DetachableMdiSubWindow* listMdiSubWindow_;
};

}

#endif
