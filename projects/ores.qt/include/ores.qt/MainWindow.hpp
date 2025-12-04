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

#include <QMainWindow>
#include <QLabel>
#include <memory>
#include <thread>
#include <boost/asio/io_context.hpp>
#include <boost/asio/executor_work_guard.hpp>
#include "ores.comms/net/client.hpp"
#include "ores.qt/MdiAreaWithBackground.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ui_MainWindow.h"

namespace Ui {

class MainWindow;

}

namespace ores::qt {

class DetachableMdiSubWindow;
class CurrencyController;

/**
 * @brief Main application window providing the MDI interface and entity
 * management.
 *
 * The MainWindow serves as the primary application window, providing:
 *
 * - MDI (Multiple Document Interface) workspace for entity windows
 * - Menu bar with File, Ref Data, Export, Window, and Help menus
 * - Toolbar with quick access to common operations
 * - Status bar showing connection state and operation messages
 * - Entity controller management for different data types (currencies,
 *   accounts, etc.)
 *
 * The window uses an entity controller pattern where each data type (currency,
 * account, trade, etc.) has its own controller that manages windows and
 * operations specific to that entity. This keeps the MainWindow clean and makes
 * it easy to add new entities.
 *
 * @note MainWindow creates entity controllers after successful login and
 * destroys them on disconnect to ensure they always have a valid client
 * connection.
 */
class MainWindow : public QMainWindow {
    Q_OBJECT

private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.main_window");
        return instance;
    }

public:
    /**
     * @brief Constructs the main window.
     * @param parent Parent widget (typically nullptr for main window)
     *
     * Initializes the UI, sets up the MDI area, configures icons, and connects
     * menu/toolbar actions. Entity controllers are created after successful
     * login.
     */
    explicit MainWindow(QWidget* parent = nullptr);

    /**
     * @brief Destroys the main window.
     *
     * Ensures all detachable windows are properly disconnected, closes the
     * client connection, and cleans up the IO context thread.
     */
    ~MainWindow() override;

    /**
     * @brief Get the connected client instance.
     * @return Shared pointer to the client, or nullptr if not connected.
     */
    std::shared_ptr<comms::net::client> getClient() const { return client_; }

protected:
    /**
     * @brief Handles the close event for the main window.
     *
     * Ensures all detachable windows (MDI and detached) are closed before
     * the main window closes, so the application can terminate cleanly.
     */
    void closeEvent(QCloseEvent* event) override;

signals:
    /**
     * @brief Emitted when server disconnect is detected by heartbeat.
     *
     * This signal is emitted from the client's disconnect callback (which runs
     * on the IO thread) and is connected to onServerDisconnectedDetected() slot
     * to safely handle the disconnect on the main thread.
     */
    void serverDisconnectedDetected();

private slots:
    /**
     * @brief Handles login action from menu/toolbar.
     *
     * Displays the login dialog and, on successful authentication, creates the
     * client connection and entity controllers.
     */
    void onLoginTriggered();

    /**
     * @brief Handles disconnect action from menu/toolbar.
     *
     * Closes all entity windows, destroys controllers, disconnects the client,
     * and stops the IO context thread.
     */
    void onDisconnectTriggered();

    /**
     * @brief Displays the About dialog.
     */
    void onAboutTriggered();

    /**
     * @brief Detaches all MDI windows to separate floating windows.
     */
    void onDetachAllTriggered();

    /**
     * @brief Handles automatic server disconnect detection from heartbeat.
     *
     * Called when the client's heartbeat mechanism detects that the server
     * has stopped responding. Performs the same cleanup as onDisconnectTriggered
     * but with a different user message indicating the disconnect was unexpected.
     */
    void onServerDisconnectedDetected();

    /**
     * @brief Updates the Window menu with the list of currently open windows.
     *
     * Called just before the Window menu is displayed. Removes old window list
     * items and adds current windows, indicating which are detached.
     */
    void onWindowMenuAboutToShow();

private:
    /**
     * @brief Updates menu and toolbar action states based on connection status.
     *
     * Enables/disables actions like Currencies, Connect/Disconnect based on
     * whether a client connection is active. Also updates the connection status
     * icon.
     */
    void updateMenuState();

    /**
     * @brief Creates all entity controllers after successful login.
     *
     * Instantiates controllers for each entity type (currency, etc.) and
     * connects their signals to the status bar for message display.
     */
    void createControllers();

private:
    /** @brief Auto-generated UI elements from MainWindow.ui */
    Ui::MainWindow* ui_;

    /** @brief MDI area with custom background for displaying entity windows */
    MdiAreaWithBackground* mdiArea_;

    /** @brief Status bar label showing connection state icon */
    QLabel* connectionStatusIconLabel_;

    /**
     * @brief List of all detachable MDI windows for detach/reattach operations.
     *
     * Shared across all entity controllers so they can track their windows for
     * the Window menu and detach/reattach operations.
     */
    QList<DetachableMdiSubWindow*> allDetachableWindows_;

    /** @brief Icon displayed in status bar when connected to server */
    QIcon connectedIcon_;

    /** @brief Icon displayed in status bar when disconnected from server */
    QIcon disconnectedIcon_;

    // Entity controllers
    /**
     * @brief Controller managing all currency-related windows and operations.
     *
     * Created after successful login, destroyed on disconnect. Handles currency
     * list, detail, and history windows.
     */
    std::unique_ptr<CurrencyController> currencyController_;

    // Client infrastructure
    /** @brief Boost ASIO IO context for async network operations */
    std::unique_ptr<boost::asio::io_context> io_context_;

    /** @brief Work guard preventing IO context from exiting when idle */
    std::unique_ptr<boost::asio::executor_work_guard<
        boost::asio::io_context::executor_type>> work_guard_;

    /** @brief Thread running the IO context for network operations */
    std::unique_ptr<std::thread> io_thread_;

    /** @brief Client connection to the server */
    std::shared_ptr<comms::net::client> client_;

    /** @brief Username of currently logged-in user */
    std::string username_;
};

}

#endif
