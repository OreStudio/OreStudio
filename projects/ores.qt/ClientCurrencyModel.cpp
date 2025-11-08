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
#include "ores.qt/ClientCurrencyModel.hpp"
#include <QtConcurrent>
#include "ores.comms/protocol/frame.hpp"
#include "ores.comms/protocol/message_types.hpp"
#include "ores.risk/messaging/protocol.hpp"

namespace ores::qt {

using namespace ores::utility::log;

ClientCurrencyModel::
ClientCurrencyModel(std::shared_ptr<comms::client> client, QObject* parent)
    : QAbstractTableModel(parent), client_(std::move(client)),
      watcher_(new QFutureWatcher < std::pair < bool,
      std::vector<risk::domain::currency>>>(this)) {

    connect(watcher_, &QFutureWatcher < std::pair < bool,
        std::vector<risk::domain::currency>>>::finished,
            this, &ClientCurrencyModel::onCurrenciesLoaded);
}

int ClientCurrencyModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(currencies_.size());
}

int ClientCurrencyModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return 10;  // Match the original model's column count
}

QVariant ClientCurrencyModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid() || role != Qt::DisplayRole)
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= currencies_.size())
        return {};

    const auto& currency = currencies_[row];

    switch (index.column()) {
    case 0: return QString::fromStdString(currency.name);
    case 1: return QString::fromStdString(currency.iso_code);
    case 2: return QString::fromStdString(currency.numeric_code);
    case 3: return QString::fromStdString(currency.symbol);
    case 4: return QString::fromStdString(currency.fraction_symbol);
    case 5: return currency.fractions_per_unit;
    case 6: return QString::fromStdString(currency.rounding_type);
    case 7: return currency.rounding_precision;
    case 8: return QString::fromStdString(currency.format);
    case 9: return QString::fromStdString(currency.currency_type);
    default: return {};
    }
}

QVariant ClientCurrencyModel::
headerData(int section, Qt::Orientation orientation, int role) const {
    if (role != Qt::DisplayRole)
        return {};

    if (orientation == Qt::Horizontal) {
        switch (section) {
        case 0: return tr("Currency Name");
        case 1: return tr("ISO Code");
        case 2: return tr("Numeric Code");
        case 3: return tr("Symbol");
        case 4: return tr("Frac. Symbol");
        case 5: return tr("Frac. per unit");
        case 6: return tr("Rounding type");
        case 7: return tr("Rounding precision");
        case 8: return tr("Format");
        case 9: return tr("Currency Type");
        default: return {};
        }
    }

    return {};
}

void ClientCurrencyModel::refresh() {
    BOOST_LOG_SEV(lg(), info) << "Calling refresh.";
    // Perform request asynchronously using QtConcurrent
    QFuture<std::pair<bool, std::vector<risk::domain::currency>>> future =
        QtConcurrent::run([this]() -> std::pair<bool, std::vector<risk::domain::currency>> {
           BOOST_LOG_SEV(lg(), info) << "Making a currencies request.";

            risk::messaging::get_currencies_request request;
            auto payload = request.serialize();

            comms::protocol::frame request_frame(
                comms::protocol::message_type::get_currencies_request,
                0,
                std::move(payload)
            );

            // Send request synchronously (on background thread)
            auto response_result = client_->send_request_sync(std::move(request_frame));

            if (!response_result) {
                return {false, {}};
            }

            BOOST_LOG_SEV(lg(), info) << "Received a currencies resposne.";
            auto response = risk::messaging::get_currencies_response::deserialize(
                response_result->payload()
            );

            if (!response) {
                return {false, {}};
            }

            return {true, std::move(response->currencies)};
        });

    watcher_->setFuture(future);
}

void ClientCurrencyModel::onCurrenciesLoaded() {
    BOOST_LOG_SEV(lg(), info) << "On currencies loaded event";
    auto [success, currencies] = watcher_->result();

    if (success) {
        beginResetModel();
        currencies_ = std::move(currencies);
        endResetModel();

        emit dataLoaded();
    } else {
        emit loadError(tr("Failed to load currencies from server"));
    }
}

const risk::domain::currency* ClientCurrencyModel::getCurrency(int row) const {
    if (row < 0 || row >= static_cast<int>(currencies_.size())) {
        return nullptr;
    }
    return &currencies_[row];
}

}
