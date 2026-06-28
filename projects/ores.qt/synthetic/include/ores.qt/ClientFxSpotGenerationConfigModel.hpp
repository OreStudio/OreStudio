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
#ifndef ORES_QT_CLIENT_FX_SPOT_GENERATION_CONFIG_MODEL_HPP
#define ORES_QT_CLIENT_FX_SPOT_GENERATION_CONFIG_MODEL_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/AbstractClientModel.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include <QFutureWatcher>
#include <vector>

namespace ores::qt {

/**
 * @brief Model for displaying FX spot generation configs fetched from the server.
 *
 * This model extends AbstractClientModel and fetches FX spot generation
 * config data asynchronously using the ores.comms client.
 */
class ClientFxSpotGenerationConfigModel final : public AbstractClientModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_fx_spot_generation_config_model";

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
        SourceName,
        OreKey,
        InitialPrice,
        TicksPerHour,
        Enabled,
        Version,
        RecordedAt,
        ColumnCount
    };

    explicit ClientFxSpotGenerationConfigModel(ClientManager* clientManager,
                                               QObject* parent = nullptr);
    ~ClientFxSpotGenerationConfigModel() override = default;

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant
    headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const override;

    /**
     * @brief Refresh FX spot generation config data from server asynchronously.
     */
    void refresh();

    /**
     * @brief Get FX spot generation config at the specified row.
     *
     * @param row The row index.
     * @return The config, or nullptr if row is invalid.
     */
    const synthetic::domain::fx_spot_generation_config* getConfig(int row) const;

    /**
     * @brief Load a specific page of data.
     */
    void load_page(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Get the page size used for pagination.
     */
    std::uint32_t page_size() const {
        return page_size_;
    }

    /**
     * @brief Set the page size for pagination.
     */
    void set_page_size(std::uint32_t size);

    /**
     * @brief Get the total number of records available on the server.
     */
    std::uint32_t total_available_count() const {
        return total_available_count_;
    }

private slots:
    void onConfigsLoaded();

private:
    struct FetchResult {
        bool success;
        std::vector<synthetic::domain::fx_spot_generation_config> configs;
        std::uint32_t total_available_count;
        QString error_message;
        QString error_details;
    };

    void fetch_configs(std::uint32_t offset, std::uint32_t limit);

    ClientManager* clientManager_;
    std::vector<synthetic::domain::fx_spot_generation_config> configs_;
    QFutureWatcher<FetchResult>* watcher_;
    std::uint32_t page_size_{100};
    std::uint32_t total_available_count_{0};
    bool is_fetching_{false};
};

}

#endif
