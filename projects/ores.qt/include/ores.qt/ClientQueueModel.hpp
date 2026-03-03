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
#ifndef ORES_QT_CLIENT_QUEUE_MODEL_HPP
#define ORES_QT_CLIENT_QUEUE_MODEL_HPP

#include <vector>
#include <QSize>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ColumnMetadata.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.mq/pgmq/queue_info.hpp"
#include "ores.mq/pgmq/queue_metrics.hpp"

namespace ores::qt {

/**
 * @brief Combined queue metadata and metrics row for the monitor table.
 *
 * Merges a queue_info and its corresponding queue_metrics by queue_name.
 * Metrics fields are absent when the server returns no metrics for a queue.
 */
struct queue_row {
    /// From queue_info
    std::string queue_name;
    std::chrono::system_clock::time_point created_at;
    bool is_unlogged{false};
    bool is_partitioned{false};

    /// From queue_metrics (may be absent if queue has no metrics yet)
    int64_t queue_length{0};
    int64_t total_messages{0};
    std::optional<int32_t> newest_msg_age_sec;
    std::optional<int32_t> oldest_msg_age_sec;
    std::optional<std::chrono::system_clock::time_point> scrape_time;
};

/**
 * @brief Table model that fetches and merges queue info and metrics.
 *
 * Issues get_queues_request and get_queue_metrics_request in sequence on a
 * background thread, joins on queue_name, and presents the merged data as a
 * read-only table. There are no create/edit/delete operations.
 */
class ClientQueueModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.client_queue_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum Column {
        QueueName,
        QueueLength,
        TotalMessages,
        NewestMsgAge,
        OldestMsgAge,
        IsUnlogged,
        IsPartitioned,
        CreatedAt,
        ScrapeTime,
        ColumnCount
    };

    static constexpr std::size_t kColumnCount = std::size_t(ColumnCount);
    static constexpr std::array<ColumnMetadata, kColumnCount> kColumns = {{
        { .column = QueueName,     .header = "Queue Name",    .style = column_style::text_left,   .hidden_by_default = false, .default_width = kColumnWidthAuto },
        { .column = QueueLength,   .header = "Length",        .style = column_style::mono_center, .hidden_by_default = false, .default_width = 80  },
        { .column = TotalMessages, .header = "Total Sent",    .style = column_style::mono_center, .hidden_by_default = false, .default_width = 90  },
        { .column = NewestMsgAge,  .header = "Newest (s)",    .style = column_style::mono_center, .hidden_by_default = false, .default_width = 85  },
        { .column = OldestMsgAge,  .header = "Oldest (s)",    .style = column_style::mono_center, .hidden_by_default = false, .default_width = 85  },
        { .column = IsUnlogged,    .header = "Unlogged",      .style = column_style::mono_center, .hidden_by_default = true,  .default_width = 75  },
        { .column = IsPartitioned, .header = "Partitioned",   .style = column_style::mono_center, .hidden_by_default = true,  .default_width = 90  },
        { .column = CreatedAt,     .header = "Created",       .style = column_style::mono_left,   .hidden_by_default = false, .default_width = kColumnWidthAuto },
        { .column = ScrapeTime,    .header = "Last Scraped",  .style = column_style::mono_left,   .hidden_by_default = false, .default_width = kColumnWidthAuto },
    }};

    inline static const QSize kDefaultWindowSize = {900, 400};
    static constexpr std::string_view kSettingsGroup = "QueueMonitorListWindow";

    static std::vector<column_style> const& columnStyles() {
        static std::vector<column_style> const kStylesVector = []() {
            std::vector<column_style> result;
            result.reserve(kColumnCount);
            for (std::size_t i = 0; i < kColumnCount; ++i)
                result.push_back(kColumns[i].style);
            return result;
        }();
        return kStylesVector;
    }

    static QVector<int> defaultHiddenColumns() {
        static QVector<int> const result =
            ::ores::qt::defaultHiddenColumns<kColumnCount>(kColumns);
        return result;
    }

    explicit ClientQueueModel(ClientManager* clientManager,
                              QObject* parent = nullptr);
    ~ClientQueueModel() override = default;

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index,
                  int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
                        int role = Qt::DisplayRole) const override;

    void refresh();

    const queue_row* getRow(int row) const;

signals:
    void dataLoaded();
    void loadError(const QString& error_message, const QString& details = {});

private slots:
    void onDataLoaded();

private:
    struct FetchResult {
        bool success;
        std::vector<queue_row> rows;
        QString error_message;
        QString error_details;
    };

    ClientManager* clientManager_;
    std::vector<queue_row> rows_;
    QFutureWatcher<FetchResult>* watcher_;
    bool is_fetching_{false};
};

}

#endif
