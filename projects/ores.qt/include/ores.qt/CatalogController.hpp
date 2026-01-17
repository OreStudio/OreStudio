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
#ifndef ORES_QT_CATALOG_CONTROLLER_HPP
#define ORES_QT_CATALOG_CONTROLLER_HPP

#include <QMainWindow>
#include <QMdiArea>
#include <QPointer>
#include "ores.qt/EntityController.hpp"
#include "ores.dq/domain/catalog.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

class CatalogMdiWindow;
class DetachableMdiSubWindow;

/**
 * @brief Controller for managing catalog-related windows.
 */
class CatalogController : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.catalog_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    CatalogController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        const QString& username,
        QList<DetachableMdiSubWindow*>& allDetachableWindows,
        QObject* parent = nullptr);

    ~CatalogController() override;

    void showListWindow() override;
    void closeAllWindows() override;

private slots:
    void onShowDetails(const dq::domain::catalog& catalog);
    void onAddNewRequested();
    void onShowHistory(const QString& name);
    void onOpenVersion(const dq::domain::catalog& catalog, int versionNumber);
    void onRevertVersion(const dq::domain::catalog& catalog);

private:
    void showAddWindow();
    void showDetailWindow(const dq::domain::catalog& catalog);
    void showHistoryWindow(const QString& name);

    QPointer<CatalogMdiWindow> listWindow_;
    QPointer<DetachableMdiSubWindow> listMdiSubWindow_;
    QList<DetachableMdiSubWindow*>& allDetachableWindows_;
};

}

#endif
