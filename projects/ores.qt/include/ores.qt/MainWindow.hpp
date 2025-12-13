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
#include <QTimer>
#include <QSystemTrayIcon>
#include <QMenu>
#include <memory>
#include <vector>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/MdiAreaWithBackground.hpp"
#include "ores.eventing/service/event_bus.hpp"
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
    inline static std::string_view logger_name = "ores.qt.main_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
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
     */
    ~MainWindow() override;

    /**
     * @brief Get the client manager.
     */
    ClientManager* getClientManager() const { return clientManager_; }

protected:
    /**
     * @brief Handles the close event for the main window.
     *
     * Ensures all detachable windows (MDI and detached) are closed before
     * the main window closes, so the application can terminate cleanly.
     */
    void closeEvent(QCloseEvent* event) override;

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

    /**
     * @brief Performs common cleanup when disconnecting from server.
     */
    void performDisconnectCleanup();

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

    /** @brief Icon displayed in status bar when reconnecting to server */
    QIcon reconnectingIcon_;

    // Entity controllers
    /**
     * @brief Controller managing all currency-related windows and operations.
     *
     * Created after successful login, destroyed on disconnect. Handles currency
     * list, detail, and history windows.
     */
    std::unique_ptr<CurrencyController> currencyController_;

    /** @brief Event bus for decoupled event handling */
    std::shared_ptr<eventing::service::event_bus> eventBus_;

    /** @brief Client manager handling network connection and IO context */
    ClientManager* clientManager_;

    /** @brief Username of currently logged-in user */
    std::string username_;

    /** @brief System tray icon for notifications */
    QSystemTrayIcon* systemTrayIcon_;

    /** @brief Context menu for the system tray icon */
    QMenu* trayContextMenu_;

    /** @brief Subscriptions to keep alive for event handling */
    std::vector<eventing::service::subscription> eventSubscriptions_;
};

}

#endif
