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
#ifndef ORES_QT_CLIENT_CATALOG_MODEL_HPP
#define ORES_QT_CLIENT_CATALOG_MODEL_HPP

#include <QAbstractTableModel>
#include <vector>
#include "ores.dq/domain/catalog.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/RecencyPulseManager.hpp"
#include "ores.qt/RecencyTracker.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Table model for displaying catalogs in a QTableView.
 *
 * Provides async loading from server with recency highlighting for
 * recently modified records. Records modified since the last reload
 * are highlighted with a pulsing color effect.
 */
class ClientCatalogModel : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.client_catalog_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum Column {
        Name = 0,
        Description,
        Owner,
        Version,
        ModifiedBy,
        RecordedAt,
        ColumnCount
    };

    explicit ClientCatalogModel(ClientManager* clientManager,
                                QObject* parent = nullptr);

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index,
                  int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
                        int role = Qt::DisplayRole) const override;

    void loadData();
    const dq::domain::catalog& catalogAt(int row) const;

signals:
    void loadStarted();
    void loadFinished();
    void errorOccurred(const QString& message, const QString& details = {});

private slots:
    void onPulseStateChanged(bool isOn);
    void onPulsingComplete();

private:
    QVariant foregroundColor(const std::string& name) const;

    ClientManager* clientManager_;
    std::vector<dq::domain::catalog> catalogs_;

    // Recency highlighting
    using CatalogKeyExtractor = std::string(*)(const dq::domain::catalog&);
    RecencyTracker<dq::domain::catalog, CatalogKeyExtractor> recencyTracker_;
    RecencyPulseManager* pulseManager_;
};

}

#endif
