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
#ifndef ORES_QT_CLIENT_BUSINESS_UNIT_TYPE_MODEL_HPP
#define ORES_QT_CLIENT_BUSINESS_UNIT_TYPE_MODEL_HPP

#include <vector>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/RecencyPulseManager.hpp"
#include "ores.qt/RecencyTracker.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/business_unit_type.hpp"

namespace ores::qt {

/**
 * @brief Model for displaying business unit types fetched from the server.
 *
 * This model extends QAbstractTableModel and fetches business unit type
 * data asynchronously using the ores.comms client.
 */
class ClientBusinessUnitTypeModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_business_unit_type_model";

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
        Code,
        Name,
        Level,
        CodingScheme,
        Description,
        Version,
        ModifiedBy,
        ColumnCount
    };

    explicit ClientBusinessUnitTypeModel(ClientManager* clientManager,
                                         QObject* parent = nullptr);
    ~ClientBusinessUnitTypeModel() override = default;

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Refresh business unit type data from server asynchronously.
     */
    void refresh();

    /**
     * @brief Get business unit type at the specified row.
     *
     * @param row The row index.
     * @return The business unit type, or nullptr if row is invalid.
     */
    const refdata::domain::business_unit_type* getType(int row) const;

    /**
     * @brief Load a specific page of data.
     */
    void load_page(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Get the page size used for pagination.
     */
    std::uint32_t page_size() const { return page_size_; }

    /**
     * @brief Set the page size for pagination.
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
    void onTypesLoaded();
    void onPulseStateChanged(bool isOn);
    void onPulsingComplete();

private:
    QVariant recency_foreground_color(const std::string& code) const;

    struct FetchResult {
        bool success;
        std::vector<refdata::domain::business_unit_type> types;
        std::uint32_t total_available_count;
        QString error_message;
        QString error_details;
    };

    void fetch_types(std::uint32_t offset, std::uint32_t limit);

    ClientManager* clientManager_;
    std::vector<refdata::domain::business_unit_type> types_;
    QFutureWatcher<FetchResult>* watcher_;
    std::uint32_t page_size_{100};
    std::uint32_t total_available_count_{0};
    bool is_fetching_{false};

    using BusinessUnitTypeKeyExtractor =
        std::string(*)(const refdata::domain::business_unit_type&);
    RecencyTracker<refdata::domain::business_unit_type,
                   BusinessUnitTypeKeyExtractor> recencyTracker_;
    RecencyPulseManager* pulseManager_;
};

}

#endif
