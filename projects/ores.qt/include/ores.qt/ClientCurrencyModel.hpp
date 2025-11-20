/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_CLIENT_CURRENCY_MODEL_HPP
#define ORES_QT_CLIENT_CURRENCY_MODEL_HPP

#include <memory>
#include <vector>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.comms/net/client.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.risk/domain/currency.hpp"

namespace ores::qt {

/**
 * @brief Model for displaying currencies fetched from the server via client.
 *
 * This model extends QAbstractTableModel and fetches currency data
 * asynchronously using the ores.comms client instead of direct database access.
 */
class ClientCurrencyModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(
            "ores.qt.client_currency_model");
        return instance;
    }

public:
    explicit ClientCurrencyModel(std::shared_ptr<comms::net::client> client,
                                   QObject* parent = nullptr);
    ~ClientCurrencyModel() override = default;

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Refresh currency data from server asynchronously.
     *
     * This method initiates an async request to fetch currencies.
     * The model will emit dataChanged() when the fetch completes.
     */
    void refresh();

    /**
     * @brief Get currency at the specified row.
     *
     * @param row The row index.
     * @return The currency object, or nullptr if row is invalid.
     */
    const risk::domain::currency* getCurrency(int row) const;

    /**
     * @brief Get all currencies.
     *
     * @return A vector containing all current currencies.
     */
    std::vector<risk::domain::currency> getCurrencies() const;

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
    void onCurrenciesLoaded();

private:
    using FutureWatcherResult = std::pair<bool,
                                          std::vector<risk::domain::currency>>;
    std::shared_ptr<comms::net::client> client_;
    std::vector<risk::domain::currency> currencies_;
    QFutureWatcher<FutureWatcherResult>* watcher_;
};

}

#endif
