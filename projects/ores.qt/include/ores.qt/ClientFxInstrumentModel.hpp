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
#ifndef ORES_QT_CLIENT_FX_INSTRUMENT_MODEL_HPP
#define ORES_QT_CLIENT_FX_INSTRUMENT_MODEL_HPP

#include <vector>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/RecencyPulseManager.hpp"
#include "ores.qt/RecencyTracker.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/fx_instrument.hpp"

namespace ores::qt {

/**
 * @brief Model for displaying FX instruments fetched from the server.
 */
class ClientFxInstrumentModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_fx_instrument_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum Column {
        Id,
        TradeType,
        BoughtCurrency,
        BoughtAmount,
        SoldCurrency,
        SoldAmount,
        ValueDate,
        Version,
        ModifiedBy,
        RecordedAt,
        ColumnCount
    };

    explicit ClientFxInstrumentModel(ClientManager* clientManager,
                                     QObject* parent = nullptr);
    ~ClientFxInstrumentModel() override = default;

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index,
        int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    void refresh();
    void load_page(std::uint32_t offset, std::uint32_t limit);

    const trading::domain::fx_instrument* getFxInstrument(int row) const;

    std::uint32_t page_size() const { return page_size_; }
    void set_page_size(std::uint32_t size);
    std::uint32_t total_available_count() const { return total_available_count_; }

signals:
    void dataLoaded();
    void loadError(const QString& error_message, const QString& details = {});

private slots:
    void onFxInstrumentsLoaded();
    void onPulseStateChanged(bool isOn);
    void onPulsingComplete();

private:
    QVariant recency_foreground_color(const std::string& key) const;

    struct FetchResult {
        bool success;
        std::vector<trading::domain::fx_instrument> instruments;
        std::uint32_t total_available_count;
        QString error_message;
        QString error_details;
    };

    void fetch_fx_instruments(std::uint32_t offset, std::uint32_t limit);

    ClientManager* clientManager_;
    std::vector<trading::domain::fx_instrument> instruments_;
    QFutureWatcher<FetchResult>* watcher_;
    std::uint32_t page_size_{100};
    std::uint32_t total_available_count_{0};
    bool is_fetching_{false};

    using KeyExtractor = std::string(*)(const trading::domain::fx_instrument&);
    RecencyTracker<trading::domain::fx_instrument, KeyExtractor> recencyTracker_;
    RecencyPulseManager* pulseManager_;
};

}

#endif
