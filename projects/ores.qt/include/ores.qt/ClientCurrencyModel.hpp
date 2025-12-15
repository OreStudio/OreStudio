/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_CLIENT_CURRENCY_MODEL_HPP
#define ORES_QT_CLIENT_CURRENCY_MODEL_HPP

#include <vector>
#include <unordered_set>
#include <chrono>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include <QTimer>
#include "ores.qt/ClientManager.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.risk/domain/currency.hpp"

namespace ores::qt {

/**
 * @brief Model for displaying currencies fetched from the server via client.
 *
 * This model extends QAbstractTableModel and fetches currency data
 * asynchronously using the ores.comms client instead of direct database access.
 */
class ClientCurrencyModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_currency_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ClientCurrencyModel(ClientManager* clientManager,
                                   QObject* parent = nullptr);
    ~ClientCurrencyModel() override = default;

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Refresh currency data from server asynchronously.
     *
     * This method initiates an async request to fetch currencies.
     * The model will emit dataChanged() when the fetch completes.
     * When replace is true, existing data is cleared before loading.
     * When false, new data is appended (for pagination).
     *
     * @param replace If true, replace existing data; if false, append.
     */
    void refresh(bool replace = true);

    /**
     * @brief Check if more data can be fetched from the server.
     *
     * @return true if there are more records available on the server
     */
    bool canFetchMore(const QModelIndex& parent = QModelIndex()) const override;

    /**
     * @brief Fetch the next page of data from the server.
     *
     * This is called automatically by Qt views when scrolling approaches
     * the end of currently loaded data.
     */
    void fetchMore(const QModelIndex& parent = QModelIndex()) override;

    /**
     * @brief Get currency at the specified row.
     *
     * @param row The row index.
     * @return The currency object, or nullptr if row is invalid.
     */
    const risk::domain::currency* getCurrency(int row) const;

    /**
     * @brief Get all currencies.
     *
     * @return A vector containing all current currencies.
     */
    std::vector<risk::domain::currency> getCurrencies() const;

    /**
     * @brief Get the page size used for pagination.
     *
     * @return The number of records fetched per page.
     */
    std::uint32_t page_size() const { return page_size_; }

    /**
     * @brief Set the page size for pagination.
     *
     * @param size The number of records to fetch per page (1-1000).
     */
    void set_page_size(std::uint32_t size);

    /**
     * @brief Get the total number of records available on the server.
     *
     * @return Total available record count.
     */
    std::uint32_t total_available_count() const { return total_available_count_; }

signals:
    /**
     * @brief Emitted when data has been successfully loaded.
     */
    void dataLoaded();

    /**
     * @brief Emitted when an error occurs during data loading.
     */
    void loadError(const QString& error_message);

private slots:
    void onCurrenciesLoaded();
    void onPulseTimerTimeout();

private:
    /**
     * @brief Calculate foreground color based on how recent the currency's valid_from date is.
     *
     * Returns a decaying highlight color for currencies with the most recent valid_from dates.
     * The color fades from highlight to transparent over the decay duration.
     *
     * @param iso_code The currency's ISO code to check for recency.
     * @return QVariant containing QColor for foreground, or invalid QVariant if no color.
     */
    QVariant recency_foreground_color(const std::string& iso_code) const;

    /**
     * @brief Update the set of recent currencies (valid_from within recency window).
     */
    void update_recent_currencies();

    /**
     * @brief Enumeration of table columns for type-safe column access.
     *
     * Using an enum instead of magic numbers makes the code self-documenting
     * and easier to refactor when columns are added, removed, or reordered.
     */
    enum Column {
        CurrencyName,
        IsoCode,
        Version,
        NumericCode,
        Symbol,
        FractionSymbol,
        FractionsPerUnit,
        RoundingType,
        RoundingPrecision,
        Format,
        CurrencyType,
        ValidFrom,
        ValidTo,
        ColumnCount  // Must be last - represents total number of columns
    };

    struct FetchResult {
        bool success;
        std::vector<risk::domain::currency> currencies;
        std::uint32_t total_available_count;
    };

    using FutureWatcherResult = FetchResult;

    ClientManager* clientManager_;
    std::vector<risk::domain::currency> currencies_;
    QFutureWatcher<FutureWatcherResult>* watcher_;
    std::uint32_t page_size_{100};
    std::uint32_t total_available_count_{0};
    bool is_fetching_{false};

    // Recency tracking for row coloring based on valid_from date
    std::unordered_set<std::string> recent_iso_codes_;  // ISO codes newer than last reload
    QDateTime last_reload_time_;  // Timestamp of last reload (for comparison)
    QTimer* pulse_timer_;
    bool pulse_state_{false};  // Toggle for pulsing effect
    int pulse_count_{0};
    static constexpr int max_pulse_cycles_{6};  // 6 pulses (3 seconds at 500ms interval)
    static constexpr int pulse_interval_ms_{500};  // Pulse toggle interval in milliseconds
};

}

#endif
