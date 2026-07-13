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
#ifndef ORES_QT_TENOR_ANCHOR_CONTROLLER_HPP
#define ORES_QT_TENOR_ANCHOR_CONTROLLER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/tenor_anchor.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/EntityController.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"
#include <QMainWindow>
#include <QMdiArea>

namespace ores::qt {

class TenorAnchorMdiWindow;
class DetachableMdiSubWindow;

/**
 * @brief Controller for managing tenor anchor windows and operations.
 *
 * Manages the lifecycle of tenor anchor list, detail, and history windows.
 * Handles event subscriptions and coordinates between windows.
 */
class TenorAnchorController final : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.tenor_anchor_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    TenorAnchorController(QMainWindow* mainWindow,
                          QMdiArea* mdiArea,
                          ClientManager* clientManager,
                          const QString& username,
                          QObject* parent = nullptr);

    void showListWindow() override;
    void closeAllWindows() override;
    void reloadListWindow() override;


signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& error);

protected:
    EntityListMdiWindow* listWindow() const override;
    void notifyOpenDialogs(const QStringList& entityIds) override;

private slots:
    void onShowDetails(const marketdata::domain::tenor_anchor& anchor);
    void onAddNewRequested();
    void onShowHistory(const marketdata::domain::tenor_anchor& anchor);
    void onRevertVersion(const marketdata::domain::tenor_anchor& anchor);
    void onOpenVersion(const marketdata::domain::tenor_anchor& anchor, int versionNumber);

private:
    void showAddWindow();
    void showDetailWindow(const marketdata::domain::tenor_anchor& anchor);
    void showHistoryWindow(const QString& code);

    TenorAnchorMdiWindow* listWindow_;
    DetachableMdiSubWindow* listMdiSubWindow_;
};

}

#endif
