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
#ifndef ORES_QT_CLIENT_SYSTEM_SETTING_MODEL_HPP
#define ORES_QT_CLIENT_SYSTEM_SETTING_MODEL_HPP

#include <vector>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/AbstractClientModel.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/RecencyPulseManager.hpp"
#include "ores.qt/RecencyTracker.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.variability.api/domain/system_setting.hpp"

namespace ores::qt {

/**
 * @brief Model for displaying system settings fetched from the server.
 *
 * This model extends QAbstractTableModel and fetches system setting data
 * asynchronously using the ores.comms client.
 */
class ClientSystemSettingModel final : public AbstractClientModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_system_setting_model";

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

    explicit ClientSystemSettingModel(ClientManager* clientManager,
                                    QObject* parent = nullptr);
    ~ClientSystemSettingModel() override = default;

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Refresh system setting data from server asynchronously.
     */
    void refresh();

    /**
     * @brief Get system setting at the specified row.
     *
     * @param row The row index.
     * @return The system setting, or nullptr if row is invalid.
     */
    const variability::domain::system_setting* getSystemSetting(int row) const;

signals:
    /**
     * @brief Emitted when data has been successfully loaded.
     */

    /**
     * @brief Emitted when an error occurs during data loading.
     */

private slots:
    void onSystemSettingsLoaded();
    void onPulseStateChanged(bool isOn);
    void onPulsingComplete();

private:
    QVariant recency_foreground_color(const std::string& name) const;

    struct FetchResult {
        bool success;
        std::vector<variability::domain::system_setting> flags;
        QString error_message;
        QString error_details;
    };

    ClientManager* clientManager_;
    std::vector<variability::domain::system_setting> flags_;
    QFutureWatcher<FetchResult>* watcher_;
    bool is_fetching_{false};

    using SystemSettingKeyExtractor = std::string(*)(const variability::domain::system_setting&);
    RecencyTracker<variability::domain::system_setting, SystemSettingKeyExtractor> recencyTracker_;
    RecencyPulseManager* pulseManager_;
};

}

#endif
