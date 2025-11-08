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
#ifndef ORES_QT_CURRENCY_MDI_WINDOW_HPP
#define ORES_QT_CURRENCY_MDI_WINDOW_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <QWidget>
#include <QTableView>
#include <QVBoxLayout>
#include <QLabel>
#include <memory>
#include "ores.comms/client.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.qt/ClientCurrencyModel.hpp"

namespace ores::qt {

/**
 * @brief MDI window for displaying currencies.
 */
class CurrencyMdiWindow : public QWidget {
    Q_OBJECT

private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.currency_mdi_window");
        return instance;
    }

public:
    explicit CurrencyMdiWindow(std::shared_ptr<comms::client> client,
                               QWidget* parent = nullptr);

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);
    void selectionChanged(bool has_selection);

public slots:
    void editSelected();
    void deleteSelected();

private slots:
    void onDataLoaded();
    void onLoadError(const QString& error_message);
    void onRowDoubleClicked(const QModelIndex& index);
    void onSelectionChanged();

private:
    QVBoxLayout* verticalLayout_;
    QTableView* currencyTableView_;
    ClientCurrencyModel* currencyModel_;
    std::shared_ptr<comms::client> client_;
};

}

#endif
