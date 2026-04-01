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
#ifndef ORES_QT_MARKET_DATA_CONTROLLER_HPP
#define ORES_QT_MARKET_DATA_CONTROLLER_HPP

#include <QMainWindow>
#include <QMdiArea>
#include <QPointer>
#include "ores.qt/EntityController.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"

namespace ores::qt {

class MarketSeriesMdiWindow;
class MarketFixingsMdiWindow;
class DetachableMdiSubWindow;

/**
 * @brief Controller managing the market data MDI windows.
 *
 * Provides two main list windows:
 *   - Market Series: all market series (yield curves, vol surfaces, etc.)
 *   - Market Fixings: index fixing series (FIXING type only)
 *
 * Each list window has a drill-down for per-series observations or fixings.
 */
class MarketDataController : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.market_data_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    MarketDataController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        const QString& username,
        QObject* parent = nullptr);

    void showListWindow() override;
    void showFixingsWindow();
    void closeAllWindows() override;
    void reloadListWindow() override;

protected:
    EntityListMdiWindow* listWindow() const override;

private slots:
    void onShowMarketObservations(
        const marketdata::domain::market_series& series);
    void onShowMarketFixings(
        const marketdata::domain::market_series& series);

private:
    void showObservationWindow(
        const marketdata::domain::market_series& series);
    void showFixingDetailWindow(
        const marketdata::domain::market_series& series);

    QPointer<MarketSeriesMdiWindow> seriesListWindow_;
    QPointer<DetachableMdiSubWindow> seriesListMdiSubWindow_;

    QPointer<MarketFixingsMdiWindow> fixingsListWindow_;
    QPointer<DetachableMdiSubWindow> fixingsListMdiSubWindow_;
};

}

#endif
