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
#include <QPointer>
#include <QMap>
#include <QString>
#include <memory>
#include "ores.comms/net/client.hpp"

namespace ores::qt {

class DetachableMdiSubWindow;

/**
 * @brief Base class for entity-specific controllers.
 *
 * Each entity (Currency, Account, Trade, etc.) has its own controller that
 * manages windows, dialogs, and operations specific to that entity. This keeps
 * MainWindow clean and makes it easy to add new entities.
 *
 * Provides generic window management functionality including window reuse,
 * tracking, and lifecycle management using namespaced keys.
 */
class EntityController : public QObject {
    Q_OBJECT

public:
    explicit EntityController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        std::shared_ptr<comms::net::client> client,
        const QString& username,
        QObject* parent = nullptr);

     ~EntityController() override = default;

    /**
     * @brief Show the main list window for this entity.
     */
    virtual void showListWindow() = 0;

    /**
     * @brief Set the client connection (e.g., after reconnecting).
     */
    virtual void setClient(std::shared_ptr<comms::net::client> client,
                           const QString& username);

    /**
     * @brief Close all windows managed by this controller.
     */
    virtual void closeAllWindows() = 0;

signals:
    /**
     * @brief Emitted when a status message should be shown.
     */
    void statusMessage(const QString& message);

    /**
     * @brief Emitted when an error message should be shown.
     */
    void errorMessage(const QString& message);

protected:
    /**
     * @brief Build a namespaced window key for tracking.
     *
     * Creates a key in the format "windowType.identifier" for consistent
     * window tracking across different entity types.
     *
     * @param windowType Type of window (e.g., "details", "history")
     * @param identifier Unique identifier (e.g., ISO code, ID)
     * @return Namespaced key string
     */
    QString build_window_key(const QString& windowType,
                             const QString& identifier) const;

    /**
     * @brief Try to reuse an existing window if it exists.
     *
     * Checks if a window with the given key exists in the managed windows map.
     * If found, brings it to front (handling both detached and MDI modes).
     *
     * @param key Window key to look up
     * @return true if window was found and reused, false otherwise
     */
    bool try_reuse_window(const QString& key);

    /**
     * @brief Bring a window to front.
     *
     * Handles both detached and MDI-attached windows, making the window
     * visible, showing it, raising it, and activating it appropriately.
     *
     * @param window Window to bring to front
     */
    void bring_window_to_front(DetachableMdiSubWindow* window);

    /**
     * @brief Track a managed window.
     *
     * Adds the window to the managed windows map for later reuse and
     * lifecycle management.
     *
     * @param key Window key for tracking
     * @param window Window to track
     */
    void track_window(const QString& key, DetachableMdiSubWindow* window);

    /**
     * @brief Untrack a managed window.
     *
     * Removes the window from the managed windows map, typically called
     * from the window's destroyed signal handler.
     *
     * @param key Window key to remove
     */
    void untrack_window(const QString& key);

    QMainWindow* mainWindow_;
    QMdiArea* mdiArea_;
    std::shared_ptr<comms::net::client> client_;
    QString username_;

    /**
     * @brief Map of all managed windows tracked by namespaced keys.
     *
     * Keys follow the pattern "windowType.identifier" (e.g.,
     * "details.USD", "history.EUR"). Uses QPointer for automatic null
     * handling when windows are destroyed externally.
     */
    QMap<QString, QPointer<DetachableMdiSubWindow>> managed_windows_;
};

}

#endif
