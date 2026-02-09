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
#include <QColor>
#include <memory>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/MdiAreaWithBackground.hpp"
#include "ores.eventing/service/event_bus.hpp"
#include "ores.logging/make_logger.hpp"
#include "ui_MainWindow.h"

namespace Ui {

class MainWindow;

}

namespace ores::connections::service {

class connection_manager;

}

namespace ores::qt {

class DetachableMdiSubWindow;
class CurrencyController;
class CountryController;
class AccountController;
class RoleController;
class TenantController;
class FeatureFlagController;
class ChangeReasonCategoryController;
class ChangeReasonController;
class OriginDimensionController;
class NatureDimensionController;
class TreatmentDimensionController;
class CodingSchemeAuthorityTypeController;
class DataDomainController;
class SubjectAreaController;
class CatalogController;
class CodingSchemeController;
class MethodologyController;
class DatasetController;
class DatasetBundleController;
class PartyTypeController;
class PartyStatusController;
class PartyIdSchemeController;
class ContactTypeController;
class PartyController;
class CounterpartyController;
class ImageCache;
class ChangeReasonCache;
class DataLibrarianWindow;

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
        using namespace ores::logging;
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

    /**
     * @brief Set instance identification info for multi-instance testing.
     *
     * @param name Instance name (e.g., "Instance 1", "Primary")
     * @param color Optional color for the instance banner
     */
    void setInstanceInfo(const QString& name, const QColor& color = QColor());

    /**
     * @brief Update the window title to reflect current state.
     *
     * Title format: "ORE Studio v{version} - {username}@{server} [Instance Name]"
     * When not connected: "ORE Studio v{version} [Instance Name]"
     */
    void updateWindowTitle();

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
     * @brief Displays the My Account dialog for user self-service.
     */
    void onMyAccountTriggered();

    /**
     * @brief Displays the session history for the current user.
     */
    void onMySessionsTriggered();

    /**
     * @brief Detaches all MDI windows to separate floating windows.
     */
    void onDetachAllTriggered();

    /**
     * @brief Handles creation of a detachable window from an entity controller.
     *
     * Adds the window to the allDetachableWindows_ list for tracking.
     * Connected to EntityController::detachableWindowCreated signal.
     *
     * @param window The newly created detachable window.
     */
    void onDetachableWindowCreated(DetachableMdiSubWindow* window);

    /**
     * @brief Handles destruction of a detachable window from an entity controller.
     *
     * Removes the window from the allDetachableWindows_ list.
     * Connected to EntityController::detachableWindowDestroyed signal.
     *
     * @param window The window being destroyed.
     */
    void onDetachableWindowDestroyed(DetachableMdiSubWindow* window);

    /**
     * @brief Updates the Window menu with the list of currently open windows.
     *
     * Called just before the Window menu is displayed. Removes old window list
     * items and adds current windows, indicating which are detached.
     */
    void onWindowMenuAboutToShow();

    // Protocol recording slots
    /**
     * @brief Toggles protocol message recording on/off.
     *
     * When enabled, records all sent/received protocol messages to a file.
     * If no recording directory is configured, prompts user to select one.
     */
    void onRecordSessionToggled(bool checked);

    /**
     * @brief Opens a file dialog to select and view a recorded session.
     */
    void onOpenRecordingTriggered();

    /**
     * @brief Opens a directory picker to set the recording output directory.
     */
    void onSetRecordingDirectoryTriggered();

    /**
     * @brief Opens the Connection Browser MDI window.
     *
     * The Connection Browser allows managing saved server connections locally
     * without requiring a server connection. This action is always enabled.
     */
    void onConnectionBrowserTriggered();

    /**
     * @brief Handles a connect request from the Connection Browser.
     *
     * Opens the Login Dialog pre-filled with the selected connection's details.
     */
    void onConnectionConnectRequested(const boost::uuids::uuid& environmentId,
                                       const QString& connectionName);

    void onModernLoginTriggered();

    /**
     * @brief Handles successful login by updating application state.
     *
     * Sets username on all controllers and updates window title.
     * Called by login dialogs after successful authentication.
     */
    void onLoginSuccess(const QString& username);

private:
    /**
     * @brief Shows the sign up dialog with pre-filled server info.
     *
     * Creates and displays a modeless SignUpDialog in an MDI subwindow.
     */
    void showSignUpDialog(const QString& host, int port);

    /**
     * @brief Shows the system provisioner wizard when in bootstrap mode.
     *
     * Displays the SystemProvisionerWizard to create the initial admin account.
     */
    void showSystemProvisionerWizard();

    /**
     * @brief Shows the tenant onboarding wizard.
     *
     * Displays the TenantOnboardingWizard for creating and provisioning
     * a new tenant, optionally seeded with GLEIF LEI data.
     */
    void showTenantOnboardingWizard();

    /**
     * @brief Options for configuring the login dialog.
     */
    struct LoginDialogOptions {
        QString host;
        int port = 5433;
        QString username;
        QString password;
        QString connectionName;       ///< If set, shown in status message on success
        bool showSavedConnections = true;  ///< Whether to populate saved connections list
        bool showSignUpButton = true;      ///< Whether to enable sign up flow
    };

    /**
     * @brief Shows the login dialog with default options.
     *
     * Creates and displays a LoginDialog in an MDI subwindow, handling all
     * signal connections for login success, bootstrap mode, etc.
     */
    void showLoginDialog();

    /**
     * @brief Shows the login dialog with the specified options.
     *
     * Creates and displays a LoginDialog in an MDI subwindow, handling all
     * signal connections for login success, bootstrap mode, etc.
     *
     * @param options Configuration options for the dialog
     */
    void showLoginDialog(const LoginDialogOptions& options);

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

    /**
     * @brief Initializes the connection manager if not already done.
     *
     * Prompts for master password if required. Returns true if the connection
     * manager was successfully initialized, false if the user cancelled or
     * an error occurred.
     */
    bool initializeConnectionManager();

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

    /** @brief Icon for recording off state (gray regular icon) */
    QIcon recordOffIcon_;

    /** @brief Icon for recording on state (red filled icon) */
    QIcon recordOnIcon_;

    // Entity controllers
    /**
     * @brief Controller managing all currency-related windows and operations.
     *
     * Created after successful login, destroyed on disconnect. Handles currency
     * list, detail, and history windows.
     */
    std::unique_ptr<CurrencyController> currencyController_;

    /**
     * @brief Controller managing all country-related windows and operations.
     *
     * Created after successful login, destroyed on disconnect. Handles country
     * list, detail, and history windows.
     */
    std::unique_ptr<CountryController> countryController_;

    /**
     * @brief Controller managing all account-related windows and operations.
     *
     * Created after successful login, handles account list and detail windows.
     * Only accessible to admin users.
     */
    std::unique_ptr<AccountController> accountController_;

    /**
     * @brief Controller managing all role-related windows and operations.
     *
     * Created after successful login, handles role list and detail windows.
     * Only accessible to admin users.
     */
    std::unique_ptr<RoleController> roleController_;

    /**
     * @brief Controller managing all tenant-related windows and operations.
     *
     * Created after successful login, handles tenant list and detail windows.
     * Only accessible to admin users.
     */
    std::unique_ptr<TenantController> tenantController_;

    /**
     * @brief Controller managing all feature flag windows and operations.
     *
     * Created after successful login, handles feature flag list and detail windows.
     * Only accessible to admin users.
     */
    std::unique_ptr<FeatureFlagController> featureFlagController_;

    /**
     * @brief Controller managing change reason category windows.
     *
     * Created after successful login, handles category list and detail windows.
     * Only accessible to admin users.
     */
    std::unique_ptr<ChangeReasonCategoryController> changeReasonCategoryController_;

    /**
     * @brief Controller managing change reason windows.
     *
     * Created after successful login, handles reason list and detail windows.
     * Only accessible to admin users.
     */
    std::unique_ptr<ChangeReasonController> changeReasonController_;

    /**
     * @brief Controller managing origin dimension windows.
     *
     * Created after successful login, handles origin dimension list, detail,
     * and history windows.
     */
    std::unique_ptr<OriginDimensionController> originDimensionController_;

    /**
     * @brief Controller managing nature dimension windows.
     *
     * Created after successful login, handles nature dimension list, detail,
     * and history windows.
     */
    std::unique_ptr<NatureDimensionController> natureDimensionController_;

    /**
     * @brief Controller managing treatment dimension windows.
     *
     * Created after successful login, handles treatment dimension list, detail,
     * and history windows.
     */
    std::unique_ptr<TreatmentDimensionController> treatmentDimensionController_;

    /**
     * @brief Controller managing coding scheme authority type windows.
     *
     * Created after successful login, handles authority type list, detail,
     * and history windows.
     */
    std::unique_ptr<CodingSchemeAuthorityTypeController> codingSchemeAuthorityTypeController_;

    /**
     * @brief Controller managing data domain windows.
     *
     * Created after successful login, handles data domain list, detail,
     * and history windows.
     */
    std::unique_ptr<DataDomainController> dataDomainController_;

    /**
     * @brief Controller managing subject area windows.
     *
     * Created after successful login, handles subject area list, detail,
     * and history windows.
     */
    std::unique_ptr<SubjectAreaController> subjectAreaController_;

    /**
     * @brief Controller managing catalog windows.
     *
     * Created after successful login, handles catalog list, detail,
     * and history windows.
     */
    std::unique_ptr<CatalogController> catalogController_;

    /**
     * @brief Controller managing coding scheme windows.
     *
     * Created after successful login, handles coding scheme list, detail,
     * and history windows.
     */
    std::unique_ptr<CodingSchemeController> codingSchemeController_;

    /**
     * @brief Controller managing methodology windows.
     *
     * Created after successful login, handles methodology list, detail,
     * and history windows.
     */
    std::unique_ptr<MethodologyController> methodologyController_;

    /**
     * @brief Controller managing dataset windows.
     *
     * Created after successful login, handles dataset list, detail,
     * and history windows.
     */
    std::unique_ptr<DatasetController> datasetController_;

    /**
     * @brief Controller managing dataset bundle windows.
     *
     * Created after successful login, handles dataset bundle list, detail,
     * and history windows.
     */
    std::unique_ptr<DatasetBundleController> datasetBundleController_;

    std::unique_ptr<PartyTypeController> partyTypeController_;
    std::unique_ptr<PartyStatusController> partyStatusController_;
    std::unique_ptr<PartyIdSchemeController> partyIdSchemeController_;
    std::unique_ptr<ContactTypeController> contactTypeController_;
    std::unique_ptr<PartyController> partyController_;
    std::unique_ptr<CounterpartyController> counterpartyController_;

    /** @brief Event bus for decoupled event handling */
    std::shared_ptr<eventing::service::event_bus> eventBus_;

    /** @brief Client manager handling network connection and IO context */
    ClientManager* clientManager_;

    /** @brief Cache for currency flag icons */
    ImageCache* imageCache_;

    /** @brief Cache for change reasons used by entity dialogs */
    ChangeReasonCache* changeReasonCache_;

    /** @brief Username of currently logged-in user */
    std::string username_;

    /** @brief System tray icon for notifications */
    QSystemTrayIcon* systemTrayIcon_;

    /** @brief Context menu for the system tray icon */
    QMenu* trayContextMenu_;

    /** @brief Subscriptions to keep alive for event handling */
    std::vector<eventing::service::subscription> eventSubscriptions_;

    /** @brief Instance name for multi-instance identification */
    QString instanceName_;

    /** @brief Instance color for the status bar indicator (invalid color means no indicator) */
    QColor instanceColor_;

    /** @brief Colored indicator in status bar showing instance color */
    QLabel* instanceColorIndicator_;

    /** @brief Event viewer MDI sub-window (nullptr if not open) */
    DetachableMdiSubWindow* eventViewerWindow_;

    /** @brief Telemetry log viewer MDI sub-window (nullptr if not open) */
    DetachableMdiSubWindow* telemetryViewerWindow_;

    /** @brief Connection manager for saved connections (client-side SQLite) */
    std::unique_ptr<connections::service::connection_manager> connectionManager_;

    /** @brief Connection Browser MDI sub-window (nullptr if not open) */
    DetachableMdiSubWindow* connectionBrowserWindow_{nullptr};

    /** @brief Data Librarian MDI sub-window (nullptr if not open) */
    DetachableMdiSubWindow* dataLibrarianWindow_{nullptr};

    /** @brief Shell MDI sub-window (nullptr if not open) */
    DetachableMdiSubWindow* shellWindow_{nullptr};

    /** @brief Name of the connection used for current login (empty if manual) */
    QString activeConnectionName_;

    /** @brief Master password for encrypting/decrypting saved passwords (session-only) */
    QString masterPassword_;
};

}

#endif
