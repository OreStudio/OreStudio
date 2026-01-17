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
#ifndef ORES_QT_CLIENT_COUNTRY_MODEL_HPP
#define ORES_QT_CLIENT_COUNTRY_MODEL_HPP

#include <vector>
#include <unordered_set>
#include <QTimer>
#include <QDateTime>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/country.hpp"

namespace ores::qt {

class ImageCache;

/**
 * @brief Model for displaying countries fetched from the server.
 *
 * This model extends QAbstractTableModel and fetches country data
 * asynchronously using the ores.comms client.
 */
class ClientCountryModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_country_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Enumeration of table columns for type-safe column access.
     */
    enum Column {
        Flag,           // Country flag icon
        Name,           // Short name
        Alpha2Code,     // ISO 3166-1 alpha-2 code
        Alpha3Code,     // ISO 3166-1 alpha-3 code
        NumericCode,    // ISO 3166-1 numeric code
        OfficialName,   // Official name
        Version,        // Version number
        RecordedBy,     // Username who recorded
        RecordedAt,     // Timestamp when recorded
        ColumnCount     // Must be last
    };

    explicit ClientCountryModel(ClientManager* clientManager,
                                ImageCache* imageCache,
                                QObject* parent = nullptr);
    ~ClientCountryModel() override = default;

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Refresh country data from server asynchronously.
     *
     * @param replace If true, replace existing data; if false, append.
     */
    void refresh(bool replace = true);

    /**
     * @brief Load a specific page of country data.
     *
     * @param offset Number of records to skip
     * @param limit Number of records to fetch
     */
    void load_page(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Check if more data can be fetched from the server.
     */
    bool canFetchMore(const QModelIndex& parent = QModelIndex()) const override;

    /**
     * @brief Fetch the next page of data from the server.
     */
    void fetchMore(const QModelIndex& parent = QModelIndex()) override;

    /**
     * @brief Get country at the specified row.
     *
     * @param row The row index.
     * @return The country object, or nullptr if row is invalid.
     */
    const refdata::domain::country* getCountry(int row) const;

    /**
     * @brief Get all countries.
     *
     * @return A vector containing all current countries.
     */
    std::vector<refdata::domain::country> getCountries() const;

    /**
     * @brief Get the page size used for pagination.
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
    void loadError(const QString& error_message, const QString& details = {});

private slots:
    void onCountriesLoaded();
    void onPulseTimerTimeout();

private:
    /**
     * @brief Calculate foreground color based on recency.
     *
     * @param alpha2_code The country's alpha-2 code to check.
     * @return QVariant containing QColor for foreground, or invalid QVariant if no color.
     */
    QVariant recency_foreground_color(const std::string& alpha2_code) const;

    /**
     * @brief Update the set of recent countries.
     */
    void update_recent_countries();

    struct FetchResult {
        bool success;
        std::vector<refdata::domain::country> countries;
        std::uint32_t total_available_count;
    };

    /**
     * @brief Internal method to fetch countries with specific offset and limit.
     */
    void fetch_countries(std::uint32_t offset, std::uint32_t limit);

    ClientManager* clientManager_;
    ImageCache* imageCache_;
    std::vector<refdata::domain::country> countries_;
    QFutureWatcher<FetchResult>* watcher_;
    std::uint32_t page_size_{100};
    std::uint32_t total_available_count_{0};
    bool is_fetching_{false};

    // Recency tracking
    std::unordered_set<std::string> recent_alpha2_codes_;
    QDateTime last_reload_time_;
    QTimer* pulse_timer_;
    bool pulse_state_{false};
    int pulse_count_{0};
    static constexpr int max_pulse_cycles_{6};
    static constexpr int pulse_interval_ms_{500};
};

}

#endif
