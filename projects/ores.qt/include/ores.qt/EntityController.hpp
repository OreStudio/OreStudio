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
#include <memory>
#include "ores.comms/net/client.hpp"

namespace ores::qt {

/**
 * @brief Base class for entity-specific controllers.
 *
 * Each entity (Currency, Account, Trade, etc.) has its own controller that
 * manages windows, dialogs, and operations specific to that entity. This keeps
 * MainWindow clean and makes it easy to add new entities.
 */
class EntityController : public QObject {
    Q_OBJECT

public:
    explicit EntityController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        std::shared_ptr<comms::client> client,
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
    virtual void setClient(std::shared_ptr<comms::client> client,
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
    QMainWindow* mainWindow_;
    QMdiArea* mdiArea_;
    std::shared_ptr<comms::client> client_;
    QString username_;
};

}

#endif
