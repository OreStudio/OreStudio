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
#ifndef ORES_QT_MAIN_WINDOW_HPP
#define ORES_QT_MAIN_WINDOW_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <QMainWindow>
#include <QLabel>
#include <memory>
#include <thread>
#include <boost/asio/io_context.hpp>
#include <boost/asio/executor_work_guard.hpp>
#include "ores.comms/client.hpp"
#include "ores.qt/MdiAreaWithBackground.hpp"
#include "ores.qt/CurrencyDetailPanel.hpp" // Include the header for CurrencyDetailPanel
#include "ores.risk/domain/currency.hpp" // Include for risk::domain::currency
#include "ores.utility/log/make_logger.hpp"
#include "ui_MainWindow.h"

namespace Ui {

class MainWindow;

}

namespace ores::qt {

class CurrencyMdiWindow;  // Forward declaration

class MainWindow : public QMainWindow {
    Q_OBJECT

private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.main_window");
        return instance;
    }

public:
    explicit MainWindow(QWidget* parent = nullptr);
    ~MainWindow() override;

    /**
     * @brief Get the connected client instance.
     * @return Shared pointer to the client, or nullptr if not connected.
     */
    std::shared_ptr<comms::client> getClient() const { return client_; }

private slots:
    void onLoginTriggered();
    void onDisconnectTriggered();
    void onSubWindowActivated(QMdiSubWindow* window);
    void onActiveWindowSelectionChanged(int selection_count);
    void onEditTriggered();
    void onDeleteTriggered();
    void onExportCSVTriggered(); // New export to CSV slot
    void onExportXMLTriggered(); // New export to XML slot
    void onShowCurrencyDetails(const risk::domain::currency& currency); // New slot
    void onCurrencyDeleted(const QString& iso_code); // Handle currency deletion
    void onAboutTriggered(); // New about dialog slot

private:
    void updateMenuState();
    void updateCrudActionState();
    QIcon createRecoloredIcon(const QString& svgPath, const QColor& color);

private:
    Ui::MainWindow* ui_;
    MdiAreaWithBackground* mdiArea_;
    CurrencyMdiWindow* activeCurrencyWindow_;
    int selectionCount_;
    CurrencyDetailPanel* currencyDetailPanel_; // New member variable
    QLabel* connectionStatusIconLabel_; // Status bar icon label
    QString displayedCurrencyIsoCode_; // Track currently displayed currency

    QIcon connectedIcon_;    // Icon for connected status
    QIcon disconnectedIcon_; // Icon for disconnected status

    // Client infrastructure
    std::unique_ptr<boost::asio::io_context> io_context_;
    std::unique_ptr<boost::asio::executor_work_guard<
        boost::asio::io_context::executor_type>> work_guard_;
    std::unique_ptr<std::thread> io_thread_;
    std::shared_ptr<comms::client> client_;
};

}

#endif
