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
#ifndef ORES_QT_CLIENT_FEATURE_FLAG_MODEL_HPP
#define ORES_QT_CLIENT_FEATURE_FLAG_MODEL_HPP

#include <vector>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/RecencyPulseManager.hpp"
#include "ores.qt/RecencyTracker.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.variability/domain/feature_flags.hpp"

namespace ores::qt {

/**
 * @brief Model for displaying feature flags fetched from the server.
 *
 * This model extends QAbstractTableModel and fetches feature flag data
 * asynchronously using the ores.comms client.
 */
class ClientFeatureFlagModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_feature_flag_model";

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
        Name,
        Enabled,
        Version,
        ModifiedBy,
        RecordedAt,
        ColumnCount
    };

    explicit ClientFeatureFlagModel(ClientManager* clientManager,
                                    QObject* parent = nullptr);
    ~ClientFeatureFlagModel() override = default;

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Refresh feature flag data from server asynchronously.
     */
    void refresh();

    /**
     * @brief Get feature flag at the specified row.
     *
     * @param row The row index.
     * @return The feature flag, or nullptr if row is invalid.
     */
    const variability::domain::feature_flags* getFeatureFlag(int row) const;

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
    void onFeatureFlagsLoaded();
    void onPulseStateChanged(bool isOn);
    void onPulsingComplete();

private:
    QVariant recency_foreground_color(const std::string& name) const;

    struct FetchResult {
        bool success;
        std::vector<variability::domain::feature_flags> flags;
        QString error_message;
        QString error_details;
    };

    ClientManager* clientManager_;
    std::vector<variability::domain::feature_flags> flags_;
    QFutureWatcher<FetchResult>* watcher_;
    bool is_fetching_{false};

    using FeatureFlagKeyExtractor = std::string(*)(const variability::domain::feature_flags&);
    RecencyTracker<variability::domain::feature_flags, FeatureFlagKeyExtractor> recencyTracker_;
    RecencyPulseManager* pulseManager_;
};

}

#endif
