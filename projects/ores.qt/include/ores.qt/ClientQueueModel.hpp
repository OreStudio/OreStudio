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
#include <optional>
#include <chrono>
#include <cstdint>
#include <string>
#include <QSize>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ColumnMetadata.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief A row in the JetStream stream monitor table.
 *
 * Populated from nats::service::jetstream_admin::list_streams() and
 * list_consumers().
 */
struct queue_row {
    std::string id;           // stream name (used as identifier)
    std::string name;         // stream name
    std::string subjects;     // subjects joined with ", "
    std::uint64_t message_count{0};
    std::uint64_t byte_count{0};
    std::uint64_t consumer_count{0};
    std::chrono::system_clock::time_point created_at;
    std::optional<std::chrono::system_clock::time_point> last_message_at;
};

/**
 * @brief Table model that lists JetStream streams and their statistics.
 *
 * Calls jetstream_admin::list_streams() on a background thread and presents
 * the results as a read-only table.
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
        StreamName,
        Subjects,
        Messages,
        Bytes,
        Consumers,
        CreatedAt,
        LastMessageAt,
        ColumnCount
    };

    static constexpr std::size_t kColumnCount = std::size_t(ColumnCount);
    static constexpr std::array<ColumnMetadata, kColumnCount> kColumns = {{
        { .column = StreamName,    .header = "Stream",        .style = column_style::text_left,   .hidden_by_default = false, .default_width = kColumnWidthAuto },
        { .column = Subjects,      .header = "Subjects",      .style = column_style::text_left,   .hidden_by_default = false, .default_width = kColumnWidthAuto },
        { .column = Messages,      .header = "Messages",      .style = column_style::mono_center, .hidden_by_default = false, .default_width = 90  },
        { .column = Bytes,         .header = "Bytes",         .style = column_style::mono_center, .hidden_by_default = false, .default_width = 90  },
        { .column = Consumers,     .header = "Consumers",     .style = column_style::mono_center, .hidden_by_default = false, .default_width = 80  },
        { .column = CreatedAt,     .header = "Created",       .style = column_style::mono_left,   .hidden_by_default = false, .default_width = kColumnWidthAuto },
        { .column = LastMessageAt, .header = "Last Message",  .style = column_style::mono_left,   .hidden_by_default = false, .default_width = kColumnWidthAuto },
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
