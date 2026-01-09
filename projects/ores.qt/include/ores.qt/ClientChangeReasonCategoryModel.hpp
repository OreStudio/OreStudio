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
#ifndef ORES_QT_CLIENT_CHANGE_REASON_CATEGORY_MODEL_HPP
#define ORES_QT_CLIENT_CHANGE_REASON_CATEGORY_MODEL_HPP

#include <vector>
#include <unordered_set>
#include <QTimer>
#include <QDateTime>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.iam/domain/change_reason_category.hpp"

namespace ores::qt {

/**
 * @brief Model for displaying change reason categories fetched from the server.
 *
 * This model extends QAbstractTableModel and fetches change reason category
 * data asynchronously using the ores.comms client.
 */
class ClientChangeReasonCategoryModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_change_reason_category_model";

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
        Description,
        Version,
        RecordedBy,
        RecordedAt,
        ColumnCount
    };

    explicit ClientChangeReasonCategoryModel(ClientManager* clientManager,
                                              QObject* parent = nullptr);
    ~ClientChangeReasonCategoryModel() override = default;

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Refresh category data from server asynchronously.
     */
    void refresh();

    /**
     * @brief Get category at the specified row.
     *
     * @param row The row index.
     * @return The category, or nullptr if row is invalid.
     */
    const iam::domain::change_reason_category* getCategory(int row) const;

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
    void onCategoriesLoaded();
    void onPulseTimerTimeout();

private:
    void update_recent_categories();
    QVariant recency_foreground_color(const std::string& code) const;

    struct FetchResult {
        bool success;
        std::vector<iam::domain::change_reason_category> categories;
    };

    ClientManager* clientManager_;
    std::vector<iam::domain::change_reason_category> categories_;
    QFutureWatcher<FetchResult>* watcher_;
    bool is_fetching_{false};

    // Recency highlighting
    QTimer* pulse_timer_;
    std::unordered_set<std::string> recent_category_codes_;
    QDateTime last_reload_time_;
    bool pulse_state_{false};
    int pulse_count_{0};
    static constexpr int pulse_interval_ms_ = 500;
    static constexpr int max_pulse_cycles_ = 6;
};

}

#endif
