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
#ifndef ORES_QT_CLIENT_MARKET_SERIES_MODEL_HPP
#define ORES_QT_CLIENT_MARKET_SERIES_MODEL_HPP

#include <vector>
#include <string>
#include <QSize>
#include <QFutureWatcher>
#include "ores.qt/AbstractClientModel.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"

namespace ores::qt {

/**
 * @brief Qt table model for market series fetched from the market data service.
 */
class ClientMarketSeriesModel final : public AbstractClientModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_market_series_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum Column {
        SeriesType,
        Metric,
        Qualifier,
        AssetClass,
        Subclass,
        IsScalar,
        Version,
        ModifiedBy,
        RecordedAt,
        ColumnCount
    };

    inline static const QSize kDefaultWindowSize = {1100, 600};
    static constexpr std::string_view kSettingsGroup = "MarketSeriesListWindow";

    explicit ClientMarketSeriesModel(ClientManager* clientManager,
                                     QObject* parent = nullptr);
    ~ClientMarketSeriesModel() override = default;

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    void refresh(bool replace = true);
    void load_page(std::uint32_t offset, std::uint32_t limit);
    void set_series_type_filter(const std::string& series_type);

    const marketdata::domain::market_series* getSeries(int row) const;
    std::uint32_t total_available_count() const { return total_available_count_; }
    std::uint32_t page_size() const { return page_size_; }
    void set_page_size(std::uint32_t size);

private slots:
    void onDataLoaded();

private:
    struct FetchResult {
        bool success;
        std::vector<marketdata::domain::market_series> entries;
        std::uint32_t total_available_count;
        QString error_message;
        QString error_details;
    };

    void fetch_data(std::uint32_t offset, std::uint32_t limit);

    ClientManager* clientManager_;
    std::vector<marketdata::domain::market_series> entries_;
    QFutureWatcher<FetchResult>* watcher_;
    std::string series_type_filter_;
    std::uint32_t page_size_{500};
    std::uint32_t total_available_count_{0};
    bool is_fetching_{false};
};

}

#endif
