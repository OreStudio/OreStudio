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
#ifndef ORES_QT_COUNTERPARTY_CONTROLLER_HPP
#define ORES_QT_COUNTERPARTY_CONTROLLER_HPP

#include <QMdiArea>
#include <QMainWindow>
#include "ores.qt/EntityController.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/counterparty.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"

namespace ores::qt {

class ChangeReasonCache;
class CounterpartyMdiWindow;
class DetachableMdiSubWindow;

/**
 * @brief Controller for managing counterparty windows and operations.
 *
 * Manages the lifecycle of counterparty list, detail, and history windows.
 * Handles event subscriptions and coordinates between windows.
 */
class CounterpartyController final : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.counterparty_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    CounterpartyController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        ImageCache* imageCache,
        ChangeReasonCache* changeReasonCache,
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

private slots:
    void onShowDetails(const refdata::domain::counterparty& counterparty);
    void onAddNewRequested();
    void onShowHistory(const refdata::domain::counterparty& counterparty);
    void onRevertVersion(const refdata::domain::counterparty& counterparty);
    void onOpenVersion(const refdata::domain::counterparty& counterparty,
                       int versionNumber);

private:
    void showAddWindow();
    void showDetailWindow(const refdata::domain::counterparty& counterparty);
    void showHistoryWindow(const refdata::domain::counterparty& counterparty);

    ImageCache* imageCache_;
    ChangeReasonCache* changeReasonCache_;
    CounterpartyMdiWindow* listWindow_;
    DetachableMdiSubWindow* listMdiSubWindow_;
};

}

#endif
