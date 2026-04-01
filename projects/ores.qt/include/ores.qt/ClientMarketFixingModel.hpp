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
#ifndef ORES_QT_CLIENT_MARKET_FIXING_MODEL_HPP
#define ORES_QT_CLIENT_MARKET_FIXING_MODEL_HPP

#include <vector>
#include <boost/uuid/uuid.hpp>
#include <QFutureWatcher>
#include "ores.qt/AbstractClientModel.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_fixing.hpp"

namespace ores::qt {

/**
 * @brief Qt table model for market fixings for a single index series.
 */
class ClientMarketFixingModel final : public AbstractClientModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_market_fixing_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum Column {
        FixingDate,
        Value,
        Source,
        RecordedAt,
        ColumnCount
    };

    inline static const QSize kDefaultWindowSize = {700, 500};

    explicit ClientMarketFixingModel(ClientManager* clientManager,
                                     const boost::uuids::uuid& series_id,
                                     QObject* parent = nullptr);
    ~ClientMarketFixingModel() override = default;

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    void refresh();
    std::uint32_t total_available_count() const { return total_available_count_; }

private slots:
    void onDataLoaded();

private:
    struct FetchResult {
        bool success;
        std::vector<marketdata::domain::market_fixing> entries;
        std::uint32_t total_available_count;
        QString error_message;
        QString error_details;
    };

    void fetch_data();

    ClientManager* clientManager_;
    boost::uuids::uuid series_id_;
    std::vector<marketdata::domain::market_fixing> entries_;
    QFutureWatcher<FetchResult>* watcher_;
    std::uint32_t total_available_count_{0};
    bool is_fetching_{false};
};

}

#endif
