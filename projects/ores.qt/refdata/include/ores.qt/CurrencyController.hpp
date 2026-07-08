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
#ifndef ORES_QT_CURRENCY_CONTROLLER_HPP
#define ORES_QT_CURRENCY_CONTROLLER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/EntityController.hpp"
#include "ores.qt/EntityListMdiWindow.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include <QMainWindow>
#include <QMdiArea>

namespace ores::qt {

class CurrencyMdiWindow;
class DetachableMdiSubWindow;
class ChangeReasonCache;
class ImageCache;

/**
 * @brief Controller for managing currency windows and operations.
 *
 * Manages the lifecycle of currency list, detail, and history windows.
 * Handles event subscriptions and coordinates between windows.
 */
class CurrencyController final : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.currency_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    CurrencyController(QMainWindow* mainWindow,
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

    /**
     * @brief Relayed from CurrencyMdiWindow; wired to
     * the target's own controller in the plugin's composition root.
     */
    void showRoundingTypesRequested();
    void showMonetaryNaturesRequested();
    void showMarketTiersRequested();

protected:
    EntityListMdiWindow* listWindow() const override;
    void notifyOpenDialogs(const QStringList& entityIds) override;

private slots:
    void onShowDetails(const refdata::domain::currency& currency);
    void onAddNewRequested();
    void onShowHistory(const refdata::domain::currency& currency);
    void onRevertVersion(const refdata::domain::currency& currency);
    void onOpenVersion(const refdata::domain::currency& currency, int versionNumber);

private:
    void showAddWindow();
    void showDetailWindow(const refdata::domain::currency& currency);
    void showHistoryWindow(const QString& code);

    ChangeReasonCache* changeReasonCache_;
    CurrencyMdiWindow* listWindow_;
    DetachableMdiSubWindow* listMdiSubWindow_;
};

}

#endif
