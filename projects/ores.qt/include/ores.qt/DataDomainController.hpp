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
#ifndef ORES_QT_DATA_DOMAIN_CONTROLLER_HPP
#define ORES_QT_DATA_DOMAIN_CONTROLLER_HPP

#include <QMdiArea>
#include <QMainWindow>
#include <QDateTime>
#include <QStringList>
#include "ores.qt/EntityController.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/data_domain.hpp"

namespace ores::qt {

class DataDomainMdiWindow;
class DetachableMdiSubWindow;

class DataDomainController final : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.data_domain_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    DataDomainController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        const QString& username,
        QObject* parent = nullptr);
    ~DataDomainController() override;

    void showListWindow() override;
    void closeAllWindows() override;
    void reloadListWindow() override;

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& error);

private slots:
    void onShowDetails(const dq::domain::data_domain& domain);
    void onAddNewRequested();
    void onShowHistory(const QString& name);
    void onRevertVersion(const dq::domain::data_domain& domain);
    void onOpenVersion(const dq::domain::data_domain& domain,
                       int versionNumber);
    void onNotificationReceived(const QString& eventType,
                                const QDateTime& timestamp,
                                const QStringList& entityIds);

private:
    void showAddWindow();
    void showDetailWindow(const dq::domain::data_domain& domain);
    void showHistoryWindow(const QString& name);

    DataDomainMdiWindow* listWindow_;
    DetachableMdiSubWindow* listMdiSubWindow_;
};

}

#endif
