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
#ifndef ORES_QT_CLIENT_TELEMETRY_LOG_MODEL_HPP
#define ORES_QT_CLIENT_TELEMETRY_LOG_MODEL_HPP

#include <vector>
#include <optional>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.telemetry/domain/telemetry_log_entry.hpp"

namespace ores::qt {

/**
 * @brief Model for displaying telemetry log entries from the server.
 *
 * This model fetches telemetry logs asynchronously, with optional filtering
 * by session ID, log level, and other criteria.
 */
class ClientTelemetryLogModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_telemetry_log_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum Column {
        Timestamp,
        Level,
        Source,
        Component,
        Tag,
        Message,
        ColumnCount
    };

    explicit ClientTelemetryLogModel(ClientManager* clientManager,
                                      QObject* parent = nullptr);
    ~ClientTelemetryLogModel() override = default;

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Load logs for a specific session.
     *
     * @param session_id The session ID to filter by.
     */
    void load_session_logs(const boost::uuids::uuid& session_id);

    /**
     * @brief Load logs within a time range.
     *
     * @param start_time Start of time range.
     * @param end_time End of time range.
     */
    void load_logs(std::chrono::system_clock::time_point start_time,
                   std::chrono::system_clock::time_point end_time);

    /**
     * @brief Load a specific page of log data.
     *
     * @param offset Number of records to skip.
     * @param limit Number of records to fetch.
     */
    void load_page(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Clear all log entries.
     */
    void clear();

    /**
     * @brief Get log entry at the specified row.
     */
    const telemetry::domain::telemetry_log_entry* get_entry(int row) const;

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
    std::uint64_t total_available_count() const { return total_available_count_; }

    /**
     * @brief Set the minimum log level filter.
     *
     * @param level Minimum level (trace, debug, info, warn, error).
     */
    void set_min_level(const std::optional<std::string>& level);

    /**
     * @brief Set the message search filter.
     *
     * @param text Text to search for in messages.
     */
    void set_message_filter(const std::optional<std::string>& text);

    /**
     * @brief Get the currently filtered session ID.
     */
    std::optional<boost::uuids::uuid> current_session_id() const {
        return current_session_id_;
    }

signals:
    void dataLoaded();
    void loadError(const QString& error_message);

private slots:
    void onLogsLoaded();

private:
    struct FetchResult {
        bool success;
        std::vector<telemetry::domain::telemetry_log_entry> entries;
        std::uint64_t total_count;
    };

    void fetch_logs();

    ClientManager* clientManager_;
    std::vector<telemetry::domain::telemetry_log_entry> entries_;
    QFutureWatcher<FetchResult>* watcher_;

    std::optional<boost::uuids::uuid> current_session_id_;
    std::chrono::system_clock::time_point start_time_;
    std::chrono::system_clock::time_point end_time_;
    std::optional<std::string> min_level_;
    std::optional<std::string> message_filter_;

    std::uint32_t page_size_{100};
    std::uint32_t current_offset_{0};
    std::uint64_t total_available_count_{0};
    bool is_fetching_{false};
};

}

#endif
