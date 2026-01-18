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
#ifndef ORES_QT_ENTITY_CONTROLLER_HPP
#define ORES_QT_ENTITY_CONTROLLER_HPP

#include <QObject>
#include <QMainWindow>
#include <QMdiArea>
#include <QString>
#include <QMap>
#include <memory>
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

class DetachableMdiSubWindow;

/**
 * @brief Abstract base class for entity controllers.
 *
 * Entity controllers manage the lifecycle of windows related to specific data
 * entities (like currencies, accounts, etc.). This base class provides common
 * functionality for managing windows, tracking them for reuse, and handling the
 * client connection.
 */
class EntityController : public QObject {
    Q_OBJECT

public:
    /**
     * @brief Constructs an entity controller.
     * @param mainWindow Parent main window.
     * @param mdiArea MDI area for displaying windows.
     * @param clientManager Client manager for network operations.
     * @param username Currently logged in user.
     * @param parent QObject parent.
     */
    EntityController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        const QString& username,
        QObject* parent = nullptr);

    virtual ~EntityController() = default;

    /**
     * @brief Updates the client manager and username (e.g. after re-login).
     */
    void setClientManager(ClientManager* clientManager, const QString& username);
    
    /**
     * @brief Updates just the username.
     */
    void setUsername(const QString& username) { username_ = username; }

    /**
     * @brief Shows the main list window for this entity.
     * Must be implemented by derived classes.
     */
    virtual void showListWindow() = 0;

    /**
     * @brief Closes all windows managed by this controller.
     * Must be implemented by derived classes.
     */
    virtual void closeAllWindows() = 0;

signals:
    /** @brief Emitted when a status message should be shown to the user. */
    void statusMessage(const QString& message);

    /** @brief Emitted when an error message should be shown to the user. */
    void errorMessage(const QString& message);

protected:
    /**
     * @brief Generates a unique key for tracking windows.
     * @param windowType Type of window (e.g., "details", "history").
     * @param identifier Unique ID of the entity (e.g., ISO code).
     * @return A string key for the window map.
     */
    QString build_window_key(const QString& windowType, const QString& identifier) const;

    /**
     * @brief Tries to reuse an existing window if one exists for the key.
     * @param key Unique window key.
     * @return true if window was found and activated, false otherwise.
     */
    bool try_reuse_window(const QString& key);

    /**
     * @brief Activates the specified window, handling detached state.
     */
    void bring_window_to_front(DetachableMdiSubWindow* window);

    /**
     * @brief Registers a window for tracking.
     */
    void track_window(const QString& key, DetachableMdiSubWindow* window);

    /**
     * @brief Unregisters a window from tracking.
     */
    void untrack_window(const QString& key);

    /**
     * @brief Shows a window in the MDI area, handling detach state.
     *
     * This method adds the window to the MDI area, removes the maximize button,
     * adjusts the size, and shows the window. If a reference window is provided
     * and is detached, the new window will also be detached and positioned
     * relative to the reference window.
     *
     * @param window The window to show.
     * @param referenceWindow Optional window to follow detach state from.
     * @param offset Position offset when detaching (default 30,30).
     */
    void show_managed_window(DetachableMdiSubWindow* window,
        DetachableMdiSubWindow* referenceWindow = nullptr,
        QPoint offset = QPoint(30, 30));

protected:
    QMainWindow* mainWindow_;
    QMdiArea* mdiArea_;
    ClientManager* clientManager_;
    QString username_;

    /** @brief Map of active windows indexed by unique key. */
    QMap<QString, DetachableMdiSubWindow*> managed_windows_;
};

}

#endif
