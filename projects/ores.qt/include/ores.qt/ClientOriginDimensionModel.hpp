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
#ifndef ORES_QT_CLIENT_ORIGIN_DIMENSION_MODEL_HPP
#define ORES_QT_CLIENT_ORIGIN_DIMENSION_MODEL_HPP

#include <vector>
#include <unordered_set>
#include <QTimer>
#include <QDateTime>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/origin_dimension.hpp"

namespace ores::qt {

/**
 * @brief Model for displaying origin dimensions fetched from the server.
 *
 * This model extends QAbstractTableModel and fetches origin dimension
 * data asynchronously using the ores.comms client.
 */
class ClientOriginDimensionModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_origin_dimension_model";

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
        Version,
        RecordedBy,
        RecordedAt,
        ColumnCount
    };

    explicit ClientOriginDimensionModel(ClientManager* clientManager,
                                         QObject* parent = nullptr);
    ~ClientOriginDimensionModel() override = default;

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Refresh dimension data from server asynchronously.
     */
    void refresh();

    /**
     * @brief Get dimension at the specified row.
     *
     * @param row The row index.
     * @return The dimension, or nullptr if row is invalid.
     */
    const dq::domain::origin_dimension* getDimension(int row) const;

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
    void onDimensionsLoaded();
    void onPulseTimerTimeout();

private:
    void update_recent_dimensions();
    QVariant recency_foreground_color(const std::string& code) const;

    struct FetchResult {
        bool success;
        std::vector<dq::domain::origin_dimension> dimensions;
    };

    ClientManager* clientManager_;
    std::vector<dq::domain::origin_dimension> dimensions_;
    QFutureWatcher<FetchResult>* watcher_;
    bool is_fetching_{false};

    // Recency highlighting
    QTimer* pulse_timer_;
    std::unordered_set<std::string> recent_dimension_codes_;
    QDateTime last_reload_time_;
    bool pulse_state_{false};
    int pulse_count_{0};
    static constexpr int pulse_interval_ms_ = 500;
    static constexpr int max_pulse_cycles_ = 6;
};

}

#endif
