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
#include "ores.qt/LoginDialog.hpp"

#include <QImage>
#include <QPixmap>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QSizePolicy>
#include <QShowEvent>
#include <QTimer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/ChangePasswordDialog.hpp"
#include "ores.qt/SignUpDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.connections/service/connection_manager.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/string_generator.hpp>

namespace ores::qt {

using namespace ores::logging;

LoginDialog::LoginDialog(ClientManager* clientManager,
                         connections::service::connection_manager* connectionManager,
                         QWidget* parent)
    : QDialog(parent),
      saved_connections_combo_(new QComboBox(this)),
      unlock_button_(new QPushButton(tr("Unlock"), this)),
      username_edit_(new QLineEdit(this)),
      password_edit_(new QLineEdit(this)),
      host_edit_(new QLineEdit(this)),
      port_spinbox_(new QSpinBox(this)),
      login_button_(new QPushButton("Login", this)),
      signup_button_(new QPushButton("Sign Up", this)),
      cancel_button_(new QPushButton("Cancel", this)),
      status_label_(new QLabel(this)),
      clientManager_(clientManager),
      connectionManager_(connectionManager) {

    setupUI();

    // Connect signals
    connect(login_button_, &QPushButton::clicked, this, &LoginDialog::onLoginClicked);
    connect(signup_button_, &QPushButton::clicked, this, &LoginDialog::onSignUpClicked);
    connect(cancel_button_, &QPushButton::clicked, this, &QDialog::reject);
    connect(unlock_button_, &QPushButton::clicked, this, &LoginDialog::onUnlockClicked);
    connect(this, &LoginDialog::loginCompleted,
            this, &LoginDialog::onLoginResult);

    // Register LoginResult for cross-thread signal/slot
    qRegisterMetaType<LoginResult>("LoginResult");
}

LoginDialog::~LoginDialog() {
}

void LoginDialog::setupUI() {
    BOOST_LOG_SEV(lg(), debug) << "Setting up UI.";

    setWindowTitle("Login to ORE Studio");
    setModal(true);
    setMinimumWidth(500);
    setFixedWidth(500); // Make dialog non-resizable horizontally
    setSizeGripEnabled(false); // Disable resize grip

    // Create form layout
    auto* form_layout = new QFormLayout();
    form_layout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);

    // Saved connections combo with unlock button
    // Always show the row, but disable combo and show unlock button if not unlocked
    auto* savedConnectionsLayout = new QHBoxLayout();
    savedConnectionsLayout->setContentsMargins(0, 0, 0, 0);
    savedConnectionsLayout->setSpacing(6);

    saved_connections_combo_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);

    const QColor iconColor(220, 220, 220);
    unlock_button_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_lock_closed_20_regular.svg", iconColor));
    unlock_button_->setToolTip(tr("Unlock saved connections"));

    savedConnectionsLayout->addWidget(saved_connections_combo_, 1);
    savedConnectionsLayout->addWidget(unlock_button_, 0);

    form_layout->addRow(tr("Saved Connection:"), savedConnectionsLayout);

    // Set up initial state based on whether connection manager is available
    if (connectionManager_) {
        // Already unlocked - populate and hide unlock button
        saved_connections_combo_->setPlaceholderText(tr("Select a saved connection..."));
        saved_connections_combo_->setEnabled(true);
        unlock_button_->setVisible(false);
        populateSavedConnections();
        connect(saved_connections_combo_, &QComboBox::currentIndexChanged,
                this, &LoginDialog::onSavedConnectionSelected);
    } else {
        // Locked - show disabled combo with unlock button
        saved_connections_combo_->setPlaceholderText(tr("Unlock to see saved connections"));
        saved_connections_combo_->setEnabled(false);
        unlock_button_->setVisible(true);
    }

    // Username field
    username_edit_->setPlaceholderText("Enter username");
    form_layout->addRow("Username:", username_edit_);

    // Password field
    password_edit_->setEchoMode(QLineEdit::Password);
    password_edit_->setPlaceholderText("Enter password");
    form_layout->addRow("Password:", password_edit_);

    // Server host field
    host_edit_->setText("localhost");
    host_edit_->setPlaceholderText("Server hostname or IP");
    form_layout->addRow("Server Host:", host_edit_);

    // Server port field
    port_spinbox_->setRange(1, 65535);
    port_spinbox_->setValue(55555);
    port_spinbox_->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed); // Match line edit width
    port_spinbox_->setMinimumWidth(username_edit_->minimumSizeHint().width()); // Match line edit width exactly
    form_layout->addRow("Server Port:", port_spinbox_);

    // Status label
    status_label_->setWordWrap(true);
    status_label_->setStyleSheet("QLabel { color: #666; font-style: italic; }");

    // Set icons on buttons (reusing iconColor from above)
    login_button_->setIcon(IconUtils::createRecoloredIcon(":/icons/ic_fluent_checkmark_20_regular.svg", iconColor));
    signup_button_->setIcon(IconUtils::createRecoloredIcon(":/icons/ic_fluent_person_add_20_regular.svg", iconColor));
    cancel_button_->setIcon(IconUtils::createRecoloredIcon(":/icons/ic_fluent_dismiss_20_regular.svg", iconColor));

    // Button layout
    auto* button_layout = new QHBoxLayout();
    button_layout->addWidget(signup_button_);
    button_layout->addStretch();
    button_layout->addWidget(cancel_button_);
    button_layout->addWidget(login_button_);

    // Main layout
    auto* main_layout = new QVBoxLayout(this);
    main_layout->addLayout(form_layout);
    main_layout->addWidget(status_label_);
    main_layout->addSpacing(10);
    main_layout->addLayout(button_layout);

    // Set default button
    login_button_->setDefault(true);
}

void LoginDialog::enableForm(bool enabled) {
    BOOST_LOG_SEV(lg(), trace) << "Enable form: " << enabled;

    username_edit_->setEnabled(enabled);
    password_edit_->setEnabled(enabled);
    host_edit_->setEnabled(enabled);
    port_spinbox_->setEnabled(enabled);
    login_button_->setEnabled(enabled);
    signup_button_->setEnabled(enabled);
}

void LoginDialog::onLoginClicked() {
    BOOST_LOG_SEV(lg(), trace) << "On login was clicked.";

    if (!clientManager_) {
        MessageBoxHelper::critical(this, "Internal Error", "Client manager not initialized");
        return;
    }

    const auto username = username_edit_->text().trimmed();
    const auto password = password_edit_->text();
    const auto host = host_edit_->text().trimmed();
    const auto port = static_cast<std::uint16_t>(port_spinbox_->value());

    // Validate input
    if (username.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a username.");
        username_edit_->setFocus();
        return;
    }

    if (password.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a password.");
        password_edit_->setFocus();
        return;
    }

    if (host.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a server host.");
        host_edit_->setFocus();
        return;
    }

    // Disable form during connection
    enableForm(false);
    status_label_->setText("Connecting to server...");
    status_label_->setStyleSheet("QLabel { color: #666; font-style: italic; }");

    // Perform login asynchronously via ClientManager
    auto* watcher = new QFutureWatcher<LoginResult>(this);
    connect(watcher, &QFutureWatcher<LoginResult>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();
        emit loginCompleted(result);
    });

    QFuture<LoginResult> future = QtConcurrent::run(
        [this, host, port, username, password]() -> LoginResult {
            return clientManager_->connectAndLogin(
                host.toStdString(), port, username.toStdString(), password.toStdString());
        }
    );

    watcher->setFuture(future);
}

void LoginDialog::onLoginResult(const LoginResult& result) {
    BOOST_LOG_SEV(lg(), debug) << "On login result called.";
    if (result.success) {
        BOOST_LOG_SEV(lg(), debug) << "Login was successful.";

        // Check if password reset is required
        if (result.password_reset_required) {
            BOOST_LOG_SEV(lg(), info) << "Password reset required, showing change password dialog.";
            status_label_->setText("Password change required...");
            status_label_->setStyleSheet("QLabel { color: #f80; font-style: italic; }");

            // Show change password dialog
            ChangePasswordDialog changeDialog(clientManager_, this);
            if (changeDialog.exec() == QDialog::Accepted) {
                BOOST_LOG_SEV(lg(), info) << "Password changed successfully.";
                status_label_->setText("Login successful!");
                status_label_->setStyleSheet("QLabel { color: #0a0; }");
                accept();  // Close dialog with success
            } else {
                BOOST_LOG_SEV(lg(), warn) << "Password change canceled or failed.";
                // Disconnect since user canceled password change
                clientManager_->disconnect();
                enableForm(true);
                status_label_->setText("");
                MessageBoxHelper::warning(this, "Password Change Required",
                    "You must change your password to continue. Please login again to retry.");
            }
        } else {
            status_label_->setText("Login successful!");
            status_label_->setStyleSheet("QLabel { color: #0a0; }");
            accept();  // Close dialog with success
        }
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Login failed: "
                                  << result.error_message.toStdString();

        enableForm(true);
        status_label_->setText("");

        MessageBoxHelper::critical(this, "Login Failed",
            QString("Authentication failed: %1").arg(result.error_message));
    }
}

void LoginDialog::onSignUpClicked() {
    BOOST_LOG_SEV(lg(), trace) << "On sign up was clicked.";

    if (!clientManager_) {
        MessageBoxHelper::critical(this, "Internal Error", "Client manager not initialized");
        return;
    }

    // Create and show signup dialog with current server info
    SignUpDialog signupDialog(clientManager_, this);
    signupDialog.setServerInfo(host_edit_->text(), port_spinbox_->value());

    if (signupDialog.exec() == QDialog::Accepted) {
        // Pre-fill the username field with the registered username
        const auto registeredUsername = signupDialog.getRegisteredUsername();
        if (!registeredUsername.isEmpty()) {
            username_edit_->setText(registeredUsername);
            password_edit_->clear();
            password_edit_->setFocus();
        }
    }
}

void LoginDialog::setConnectionDetails(const QString& host, int port,
                                        const QString& username, const QString& password) {

    BOOST_LOG_SEV(lg(), debug) << "Pre-filling connection details for host: "
                               << host.toStdString();

    host_edit_->setText(host);
    port_spinbox_->setValue(port);
    username_edit_->setText(username);
    password_edit_->setText(password);

    // If password is empty, focus on password field
    // Otherwise, user can just click Login
    if (password.isEmpty()) {
        password_edit_->setFocus();
    } else {
        login_button_->setFocus();
    }
}

void LoginDialog::showEvent(QShowEvent* event) {
    QDialog::showEvent(event);

    if (autoSubmit_) {
        // Check if all fields are filled
        const bool allFieldsFilled =
            !username_edit_->text().trimmed().isEmpty() &&
            !password_edit_->text().isEmpty() &&
            !host_edit_->text().trimmed().isEmpty();

        if (allFieldsFilled) {
            BOOST_LOG_SEV(lg(), debug) << "Auto-submit enabled with all fields filled, triggering login";
            // Use single-shot timer to trigger login after dialog is fully shown
            QTimer::singleShot(0, this, &LoginDialog::onLoginClicked);
        } else {
            BOOST_LOG_SEV(lg(), debug) << "Auto-submit enabled but not all fields filled";
        }

        // Reset auto-submit flag to avoid re-triggering
        autoSubmit_ = false;
    }
}

void LoginDialog::populateSavedConnections() {
    if (!connectionManager_) {
        return;
    }

    saved_connections_combo_->clear();
    saved_connections_combo_->addItem("", QVariant()); // Empty option

    try {
        auto environments = connectionManager_->get_all_environments();
        for (const auto& env : environments) {
            QString displayName = QString::fromStdString(env.name);
            QString idStr = QString::fromStdString(boost::uuids::to_string(env.id));
            saved_connections_combo_->addItem(displayName, idStr);
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load saved connections: " << e.what();
    }
}

void LoginDialog::onSavedConnectionSelected(int index) {
    if (!connectionManager_ || index <= 0) {
        return; // Index 0 is the empty placeholder
    }

    QString idStr = saved_connections_combo_->itemData(index).toString();
    if (idStr.isEmpty()) {
        return;
    }

    try {
        boost::uuids::string_generator gen;
        boost::uuids::uuid envId = gen(idStr.toStdString());

        auto env = connectionManager_->get_environment(envId);
        if (env) {
            host_edit_->setText(QString::fromStdString(env->host));
            port_spinbox_->setValue(env->port);
            username_edit_->setText(QString::fromStdString(env->username));

            // Try to get the password
            try {
                std::string password = connectionManager_->get_password(envId);
                password_edit_->setText(QString::fromStdString(password));
            } catch (...) {
                password_edit_->clear();
            }

            BOOST_LOG_SEV(lg(), debug) << "Selected saved connection: " << env->name;

            // Focus password field if empty, otherwise login button
            if (password_edit_->text().isEmpty()) {
                password_edit_->setFocus();
            } else {
                login_button_->setFocus();
            }
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load connection: " << e.what();
    }
}

void LoginDialog::setUnlockCallback(UnlockConnectionsCallback callback) {
    unlockCallback_ = std::move(callback);
}

void LoginDialog::onUnlockClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Unlock button clicked";

    if (!unlockCallback_) {
        BOOST_LOG_SEV(lg(), warn) << "No unlock callback set";
        return;
    }

    // Call the unlock callback to get the connection manager
    auto* mgr = unlockCallback_();
    if (!mgr) {
        BOOST_LOG_SEV(lg(), debug) << "Unlock cancelled or failed";
        return;
    }

    // Store the connection manager and update UI
    connectionManager_ = mgr;

    // Enable and populate the combo
    saved_connections_combo_->setPlaceholderText(tr("Select a saved connection..."));
    saved_connections_combo_->setEnabled(true);
    unlock_button_->setVisible(false);

    // Connect the selection signal
    connect(saved_connections_combo_, &QComboBox::currentIndexChanged,
            this, &LoginDialog::onSavedConnectionSelected);

    // Populate the combo with saved connections
    populateSavedConnections();

    BOOST_LOG_SEV(lg(), info) << "Saved connections unlocked";
}

}