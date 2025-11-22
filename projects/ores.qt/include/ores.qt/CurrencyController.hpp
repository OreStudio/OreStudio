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
#ifndef ORES_QT_CURRENCY_CONTROLLER_HPP
#define ORES_QT_CURRENCY_CONTROLLER_HPP

#include <QPointer>
#include <QList>
#include "ores.qt/EntityController.hpp"
#include "ores.risk/domain/currency.hpp"
#include "ores.utility/log/make_logger.hpp"

namespace ores::qt {

class DetachableMdiSubWindow;

/**
 * @brief Controller managing all currency-related windows and operations.
 *
 * The CurrencyController encapsulates all currency-specific functionality,
 * including:
 *
 * - Currency list window showing all currencies in the system
 * - Currency detail dialogs for creating/editing currencies
 * - Currency history dialogs showing version history
 * - Window lifecycle management (creation, tracking, cleanup)
 *
 * This controller follows the entity controller pattern where MainWindow
 * delegates all currency operations to this controller, keeping the main window
 * clean and entity-agnostic.
 *
 * @note The controller maintains weak references (QPointer) to windows to allow
 * them to be closed independently without leaving dangling pointers.
 */
class CurrencyController : public EntityController {
    Q_OBJECT

private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.currency_controller");
        return instance;
    }

public:
    /**
     * @brief Constructs the currency controller.
     *
     * @param mainWindow Parent main window (for dialog ownership)
     * @param mdiArea MDI area where windows will be displayed
     * @param client Shared client connection for server communication
     * @param username Username of logged-in user (for audit trails)
     * @param allDetachableWindows Reference to MainWindow's window list
     * for detach/reattach operations
     * @param parent QObject parent (for Qt ownership)
     */
    explicit CurrencyController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        std::shared_ptr<comms::net::client> client,
        const QString& username,
        QList<DetachableMdiSubWindow*>& allDetachableWindows,
        QObject* parent = nullptr);

    /**
     * @brief Destroys the currency controller.
     *
     * All windows owned by this controller are automatically cleaned up through
     * Qt's parent-child ownership.
     */
    ~CurrencyController() override;

    /**
     * @brief Shows the currency list window.
     *
     * If the window already exists, brings it to front. Otherwise, creates a
     * new currency list window displaying all currencies from the server.
     *
     * @note Inherited from EntityController
     */
    void showListWindow() override;

    /**
     * @brief Closes all windows managed by this controller.
     *
     * Called when disconnecting from the server to clean up all currency
     * windows before destroying the controller.
     *
     * @note Inherited from EntityController
     */
    void closeAllWindows() override;

private slots:
    /**
     * @brief Handles request to add a new currency.
     *
     * Creates and displays a currency detail dialog for entering a new
     * currency. Connected to the currency list window's add button.
     */
    void onAddNewRequested();

    /**
     * @brief Handles request to show currency details.
     *
     * Creates and displays a currency detail dialog for viewing/editing an
     * existing currency. Connected to the currency list window's edit button
     * and double-click.
     *
     * @param currency The currency to display/edit
     */
    void onShowCurrencyDetails(const risk::domain::currency& currency);

    /**
     * @brief Handles request to show currency version history.
     *
     * Creates and displays a currency history dialog showing all versions of
     * the specified currency. Connected to the currency list window's history
     * button.
     *
     * @param isoCode ISO code of the currency to show history for
     */
    void onShowCurrencyHistory(const QString& isoCode);

private:
    /**
     * @brief Reference to MainWindow's list of all detachable windows.
     *
     * Windows created by this controller are added to this list so they
     * can participate in detach/reattach operations and appear in the
     * Window menu.
     */
    QList<DetachableMdiSubWindow*>& allDetachableWindows_;

    /**
     * @brief Weak pointer to the currency list window.
     *
     * Uses QPointer to automatically become null if the window is closed
     * externally. This allows reusing the existing window if it exists, or
     * creating a new one if it was closed.
     */
    QPointer<DetachableMdiSubWindow> currencyListWindow_;

    /**
     * @brief Map of currency history windows by ISO code.
     *
     * Tracks existing history windows to reuse them instead of creating
     * duplicates. Uses QPointer to handle external deletion gracefully.
     */
    QMap<QString, QPointer<DetachableMdiSubWindow>> currencyHistoryWindows_;
};

}

#endif
