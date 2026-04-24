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

#include <QList>
#include <QMenu>
#include <QColor>
#include <QLabel>
#include <QTimer>
#include <QPointer>
#include <QMainWindow>
#include <QSystemTrayIcon>
#include <memory>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/MdiAreaWithBackground.hpp"
#include "ores.eventing/service/event_bus.hpp"
#include "ores.logging/make_logger.hpp"

namespace Ui {

class MainWindow;

}

namespace ores::connections::service {

class connection_manager;

}

namespace ores::qt {

class DetachableMdiSubWindow;
class ImageCache;
class ChangeReasonCache;
class BadgeCache;

/**
 * @brief Main application window providing the MDI interface.
 *
 * Owns shared infrastructure (ClientManager, caches, event bus) and drives the
 * plugin lifecycle.  Domain entity management is delegated to AdminPlugin,
 * domain-specific plugins via the IPlugin interface.
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
     */
    void setInstanceInfo(const QString& name, const QColor& color = QColor());

    /**
     * @brief Set the HTTP base URL for compute service file uploads.
     */
    void setHttpBaseUrl(const std::string& url);

    /**
     * @brief Update the window title to reflect current state.
     */
    void updateWindowTitle();

protected:
    void closeEvent(QCloseEvent* event) override;

    bool eventFilter(QObject* watched, QEvent* event) override;

private slots:
    void onLoginTriggered();
    void onDisconnectTriggered();
    void onAboutTriggered();
    void onMyAccountTriggered();
    void onMySessionsTriggered();
    void onDetachAllTriggered();
    void onDetachableWindowCreated(DetachableMdiSubWindow* window);
    void onDetachableWindowDestroyed(DetachableMdiSubWindow* window);
    void onWindowMenuAboutToShow();
    void onRecordSessionToggled(bool checked);
    void onOpenRecordingTriggered();
    void onSetRecordingDirectoryTriggered();
    void onConnectionBrowserTriggered();
    void onConnectionConnectRequested(const boost::uuids::uuid& connectionId,
                                       const QString& connectionName);
    void onEnvironmentConnectRequested(const boost::uuids::uuid& environmentId,
                                       const QString& environmentName);
    void onModernLoginTriggered();
    void onLoginSuccess(const QString& username);

private:
    void showSignUpDialog(const QString& host, int port);
    void showSystemProvisionerWizard(
        const QString& username = {}, const QString& password = {});
    void showTenantProvisioningWizard();
    void showPartyProvisioningWizard();

    struct LoginDialogOptions {
        QString host;
        int port = 4222;
        QString username;
        QString password;
        QString connectionName;
        bool showSavedConnections = true;
        bool showSignUpButton = true;
    };

    void showLoginDialog();
    void showLoginDialog(const LoginDialogOptions& options);
    void updateMenuState();
    void updateStatusBarFields();
    QString buildConnectionTooltip() const;
    void performDisconnectCleanup();
    bool initializeConnectionManager();

private:
    Ui::MainWindow* ui_;

    MdiAreaWithBackground* mdiArea_;
    QLabel* connectionStatusIconLabel_;

    QWidget* userStatusWidget_;
    QLabel*  userStatusNameLabel_;
    QWidget* serverStatusWidget_;
    QLabel*  serverStatusNameLabel_;
    QWidget* tenantStatusWidget_;
    QLabel*  tenantStatusNameLabel_;
    QWidget* partyStatusWidget_;
    QLabel*  partyStatusNameLabel_;

    QList<QPointer<DetachableMdiSubWindow>> allDetachableWindows_;

    QIcon connectedIcon_;
    QIcon disconnectedIcon_;
    QIcon reconnectingIcon_;
    QIcon recordOffIcon_;
    QIcon recordOnIcon_;

    /** @brief Menus inserted into the menu bar by plugins; removed on logout. */
    QList<QMenu*> plugin_menus_;

    /** @brief Toolbar actions contributed by plugins (used for enable/disable). */
    QList<QAction*> plugin_toolbar_actions_;

    std::shared_ptr<eventing::service::event_bus> eventBus_;
    ClientManager* clientManager_;
    ImageCache* imageCache_;
    ChangeReasonCache* changeReasonCache_;
    BadgeCache* badgeCache_;

    std::string username_;
    std::string httpBaseUrl_;
    QString party_name_;

    QSystemTrayIcon* systemTrayIcon_;
    QMenu* trayContextMenu_;

    std::vector<eventing::service::subscription> eventSubscriptions_;

    QString instanceName_;
    QColor instanceColor_;
    QLabel* instanceColorIndicator_;

    DetachableMdiSubWindow* eventViewerWindow_;
    DetachableMdiSubWindow* telemetryViewerWindow_;

    std::unique_ptr<connections::service::connection_manager> connectionManager_;

    DetachableMdiSubWindow* connectionBrowserWindow_{nullptr};
    DetachableMdiSubWindow* shellWindow_{nullptr};
    DetachableMdiSubWindow* mySessionsWindow_{nullptr};
    DetachableMdiSubWindow* myAccountWindow_{nullptr};
    DetachableMdiSubWindow* aboutSubWindow_{nullptr};

    QString activeConnectionName_;
    QString masterPassword_;
};

}

#endif
