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
#ifndef ORES_QT_CLIENT_BUSINESS_CENTRE_MODEL_HPP
#define ORES_QT_CLIENT_BUSINESS_CENTRE_MODEL_HPP

#include <vector>
#include <cstdint>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/RecencyPulseManager.hpp"
#include "ores.qt/RecencyTracker.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/business_centre.hpp"

namespace ores::qt {

/**
 * @brief Model for displaying business centres fetched from the server.
 *
 * This model extends QAbstractTableModel and fetches business centre
 * data asynchronously using the ores.comms client.
 */
class ClientBusinessCentreModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_business_centre_model";

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
        Flag,
        Code,
        Source,
        Description,
        CodingScheme,
        CountryAlpha2,
        Version,
        RecordedBy,
        RecordedAt,
        ColumnCount
    };

    explicit ClientBusinessCentreModel(ClientManager* clientManager,
                                       ImageCache* imageCache,
                                       QObject* parent = nullptr);
    ~ClientBusinessCentreModel() override = default;

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Refresh business centre data from server asynchronously.
     *
     * When replace is true, existing data is cleared before loading.
     * When false, new data is appended (for pagination).
     *
     * @param replace If true, replace existing data; if false, append.
     */
    void refresh(bool replace = true);

    /**
     * @brief Load a specific page of business centre data.
     *
     * @param offset Number of records to skip
     * @param limit Number of records to fetch
     */
    void load_page(std::uint32_t offset, std::uint32_t limit);

    bool canFetchMore(const QModelIndex& parent = QModelIndex()) const override;
    void fetchMore(const QModelIndex& parent = QModelIndex()) override;

    /**
     * @brief Get business centre at the specified row.
     *
     * @param row The row index.
     * @return The business centre, or nullptr if row is invalid.
     */
    const refdata::domain::business_centre* getBusinessCentre(int row) const;

    std::uint32_t page_size() const { return page_size_; }
    void set_page_size(std::uint32_t size);
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
    void onBusinessCentresLoaded();
    void onPulseStateChanged(bool isOn);
    void onPulsingComplete();

private:
    QVariant recency_foreground_color(const std::string& code) const;

    struct FetchResult {
        bool success;
        std::vector<refdata::domain::business_centre> business_centres;
        std::uint32_t total_available_count;
        QString error_message;
        QString error_details;
    };

    void fetch_business_centres(std::uint32_t offset, std::uint32_t limit);

    ClientManager* clientManager_;
    ImageCache* imageCache_;
    std::vector<refdata::domain::business_centre> business_centres_;
    QFutureWatcher<FetchResult>* watcher_;
    std::uint32_t page_size_{100};
    std::uint32_t total_available_count_{0};
    bool is_fetching_{false};

    using BusinessCentreKeyExtractor = std::string(*)(const refdata::domain::business_centre&);
    RecencyTracker<refdata::domain::business_centre, BusinessCentreKeyExtractor> recencyTracker_;
    RecencyPulseManager* pulseManager_;
};

}

#endif
