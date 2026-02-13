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
#ifndef ORES_QT_CLIENT_PARTY_STATUS_MODEL_HPP
#define ORES_QT_CLIENT_PARTY_STATUS_MODEL_HPP

#include <vector>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/RecencyPulseManager.hpp"
#include "ores.qt/RecencyTracker.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/party_status.hpp"

namespace ores::qt {

/**
 * @brief Model for displaying party statuses fetched from the server.
 *
 * This model extends QAbstractTableModel and fetches party status
 * data asynchronously using the ores.comms client.
 */
class ClientPartyStatusModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_party_status_model";

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
        Description,
        DisplayOrder,
        Version,
        ModifiedBy,
        RecordedAt,
        ColumnCount
    };

    explicit ClientPartyStatusModel(ClientManager* clientManager,
                                       QObject* parent = nullptr);
    ~ClientPartyStatusModel() override = default;

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Refresh party status data from server asynchronously.
     */
    void refresh();

    /**
     * @brief Get party status at the specified row.
     *
     * @param row The row index.
     * @return The party status, or nullptr if row is invalid.
     */
    const refdata::domain::party_status* getStatus(int row) const;

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
    void onStatussLoaded();
    void onPulseStateChanged(bool isOn);
    void onPulsingComplete();

private:
    QVariant recency_foreground_color(const std::string& code) const;

    struct FetchResult {
        bool success;
        std::vector<refdata::domain::party_status> statuses;
        QString error_message;
        QString error_details;
    };

    ClientManager* clientManager_;
    std::vector<refdata::domain::party_status> statuses_;
    QFutureWatcher<FetchResult>* watcher_;
    bool is_fetching_{false};

    using PartyStatusKeyExtractor = std::string(*)(const refdata::domain::party_status&);
    RecencyTracker<refdata::domain::party_status, PartyStatusKeyExtractor> recencyTracker_;
    RecencyPulseManager* pulseManager_;
};

}

#endif
