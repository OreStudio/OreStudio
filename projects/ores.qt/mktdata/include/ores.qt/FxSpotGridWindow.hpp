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
#ifndef ORES_QT_FX_SPOT_GRID_WINDOW_HPP
#define ORES_QT_FX_SPOT_GRID_WINDOW_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.client/fx_spot_subscription.hpp"
#include "ores.qt/ClientManager.hpp"
#include <QColor>
#include <QFutureWatcher>
#include <QTableWidget>
#include <QTimer>
#include <QWidget>
#include <chrono>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace ores::qt {

/**
 * @brief Live FX spot grid showing one row per market data series.
 *
 * Rows are populated from the marketdata series registry (series_type "FX",
 * subclass spot). Each row subscribes to the official tenant tick stream for
 * its ORE key and displays live mid, a 24h-change placeholder (see task
 * fx_spot_grid_24h_change), and a staleness-derived status indicator that
 * flashes green/red on each tick.
 */
class FxSpotGridWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.fx_spot_grid_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    enum Column { ColPair = 0, ColMid, ColChange, ColStatus, ColumnCount };

    enum class FeedStatus { Pending, Live, Stale, Disconnected };

    struct RowState {
        int row = -1;
        std::string ore_key;
        double last_mid = 0.0;
        bool ever_ticked = false;
        std::chrono::system_clock::time_point last_tick{};
        std::unique_ptr<marketdata::client::fx_spot_subscription> subscription;
    };

    struct LoadResult {
        bool success = false;
        QString error;
        std::vector<marketdata::domain::market_series> series;
    };

public:
    explicit FxSpotGridWindow(ClientManager* clientManager, QWidget* parent = nullptr);
    ~FxSpotGridWindow() override = default;

    QSize sizeHint() const override {
        return {560, 280};
    }

    void reload();

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

private slots:
    void onLoadFinished();
    void onStaleCheck();

private:
    void setupUi();
    void buildRows(const std::vector<marketdata::domain::market_series>& series);
    void subscribe(RowState& rs);
    void applyTick(const std::string& ore_key, double mid,
                   std::chrono::system_clock::time_point when);
    void updateStatusCell(RowState& rs, FeedStatus status);
    static FeedStatus deriveStatus(const RowState& rs);
    static QString statusText(FeedStatus s);
    static QColor statusColor(FeedStatus s);

    ClientManager* clientManager_;
    QTableWidget* table_;
    QTimer* staleTimer_;
    QFutureWatcher<LoadResult>* loadWatcher_;
    std::map<std::string, RowState> rows_; // keyed by ore_key
};

} // namespace ores::qt

#endif
