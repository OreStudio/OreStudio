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
#ifndef ORES_QT_CLIENT_DATA_DOMAIN_MODEL_HPP
#define ORES_QT_CLIENT_DATA_DOMAIN_MODEL_HPP

#include <vector>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/RecencyPulseManager.hpp"
#include "ores.qt/RecencyTracker.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/data_domain.hpp"

namespace ores::qt {

class ClientDataDomainModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_data_domain_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum Column {
        Name,
        Description,
        Version,
        ModifiedBy,
        RecordedAt,
        ColumnCount
    };

    explicit ClientDataDomainModel(ClientManager* clientManager,
                                   QObject* parent = nullptr);
    ~ClientDataDomainModel() override = default;

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    void refresh();
    const dq::domain::data_domain* getDomain(int row) const;

signals:
    void dataLoaded();
    void loadError(const QString& error_message, const QString& details = {});

private slots:
    void onDomainsLoaded();
    void onPulseStateChanged(bool isOn);
    void onPulsingComplete();

private:
    QVariant recency_foreground_color(const std::string& name) const;

    struct FetchResult {
        bool success;
        std::vector<dq::domain::data_domain> domains;
        QString error_message;
        QString error_details;
    };

    ClientManager* clientManager_;
    std::vector<dq::domain::data_domain> domains_;
    QFutureWatcher<FetchResult>* watcher_;
    bool is_fetching_{false};

    using DataDomainKeyExtractor = std::string(*)(const dq::domain::data_domain&);
    RecencyTracker<dq::domain::data_domain, DataDomainKeyExtractor> recencyTracker_;
    RecencyPulseManager* pulseManager_;
};

}

#endif
