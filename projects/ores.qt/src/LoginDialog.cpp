/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/DialogStyles.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ChangePasswordDialog.hpp"
#include "ores.utility/version/version.hpp"
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QKeyEvent>
#include <QtConcurrent>
#include <QFutureWatcher>

namespace ores::qt {

using namespace ores::logging;

namespace {

const QString titleStyle = R"(
    QLabel {
        background: transparent;
        color: #ffffff;
        font-size: 32px;
        font-weight: bold;
        letter-spacing: 2px;
    }
)";

}

LoginDialog::LoginDialog(QWidget* parent)
    : QWidget(parent) {
    setupUI();

    // Register LoginResult for cross-thread signal/slot
    qRegisterMetaType<LoginResult>("LoginResult");
}

LoginDialog::~LoginDialog() = default;

QSize LoginDialog::sizeHint() const {
    return {400, 520};
}

void LoginDialog::keyPressEvent(QKeyEvent* event) {
    if (event->key() == Qt::Key_Escape) {
        emit closeRequested();
    } else {
        QWidget::keyPressEvent(event);
    }
}

void LoginDialog::setSavedConnections(const QStringList& connectionNames) {
    savedConnectionsMenu_->clear();

    if (connectionNames.isEmpty()) {
        savedConnectionsButton_->setVisible(false);
        return;
    }

    savedConnectionsButton_->setVisible(true);

    for (const QString& name : connectionNames) {
        auto* action = savedConnectionsMenu_->addAction(name);
        connect(action, &QAction::triggered, this, [this, name]() {
            emit savedConnectionSelected(name);
        });
    }
}

void LoginDialog::setServer(const QString& server) {
    hostEdit_->setText(server);
}

void LoginDialog::setPort(int port) {
    portSpinBox_->setValue(port);
}

void LoginDialog::setUsername(const QString& username) {
    usernameEdit_->setText(username);
}

void LoginDialog::setPassword(const QString& password) {
    passwordEdit_->setText(password);
}

void LoginDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

QString LoginDialog::getUsername() const {
    return usernameEdit_->text().trimmed();
}

QString LoginDialog::getServer() const {
    return hostEdit_->text().trimmed();
}

int LoginDialog::getPort() const {
    return portSpinBox_->value();
}

void LoginDialog::enableForm(bool enabled) {
    usernameEdit_->setEnabled(enabled);
    passwordEdit_->setEnabled(enabled);
    hostEdit_->setEnabled(enabled);
    portSpinBox_->setEnabled(enabled);
    loginButton_->setEnabled(enabled);
    signUpButton_->setEnabled(enabled);
    savedConnectionsButton_->setEnabled(enabled);
}

void LoginDialog::setupUI() {
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);
    mainLayout->setSpacing(0);

    // Main panel
    auto* mainPanel = new QWidget(this);
    mainPanel->setObjectName("mainPanel");
    mainPanel->setStyleSheet(dialog_styles::panel);
    setupRightPanel(mainPanel);

    mainLayout->addWidget(mainPanel);
}

void LoginDialog::setupLeftPanel(QWidget*) {
    // No longer used - single panel design
}

void LoginDialog::setupRightPanel(QWidget* parent) {
    auto* layout = new QVBoxLayout(parent);
    layout->setContentsMargins(36, 24, 36, 16);
    layout->setSpacing(0);

    layout->addStretch(1);
    setupHeader(layout, parent);
    setupAuthFields(layout, parent);
    setupServerFields(layout, parent);
    setupActions(layout, parent);
    layout->addStretch(1);
    setupFooter(layout, parent);
}

void LoginDialog::setupHeader(QVBoxLayout* layout, QWidget* parent) {
    loginTitleLabel_ = new QLabel("ORE STUDIO", parent);
    loginTitleLabel_->setStyleSheet(titleStyle);
    layout->addWidget(loginTitleLabel_, 0, Qt::AlignCenter);
    layout->addSpacing(24);
}

void LoginDialog::setupAuthFields(QVBoxLayout* layout, QWidget* parent) {
    // Username field
    auto* usernameLabel = new QLabel("USERNAME", parent);
    usernameLabel->setStyleSheet(dialog_styles::field_label);
    layout->addWidget(usernameLabel);
    layout->addSpacing(4);

    usernameEdit_ = new QLineEdit(parent);
    usernameEdit_->setPlaceholderText("Enter your username");
    usernameEdit_->setStyleSheet(dialog_styles::input_field);
    usernameEdit_->setFixedHeight(36);
    layout->addWidget(usernameEdit_);

    layout->addSpacing(12);

    // Password field
    auto* passwordLabel = new QLabel("PASSWORD", parent);
    passwordLabel->setStyleSheet(dialog_styles::field_label);
    layout->addWidget(passwordLabel);
    layout->addSpacing(4);

    passwordEdit_ = new QLineEdit(parent);
    passwordEdit_->setPlaceholderText("Enter your password");
    passwordEdit_->setEchoMode(QLineEdit::Password);
    passwordEdit_->setStyleSheet(dialog_styles::input_field);
    passwordEdit_->setFixedHeight(36);
    layout->addWidget(passwordEdit_);

    // Options row
    layout->addSpacing(8);
    auto* optionsRow = new QHBoxLayout();
    showPasswordCheck_ = new QCheckBox("Show password", parent);
    showPasswordCheck_->setStyleSheet(dialog_styles::checkbox);
    connect(showPasswordCheck_, &QCheckBox::toggled,
            this, &LoginDialog::onShowPasswordToggled);

    rememberMeCheck_ = new QCheckBox("Remember me", parent);
    rememberMeCheck_->setStyleSheet(dialog_styles::checkbox);

    optionsRow->addWidget(showPasswordCheck_);
    optionsRow->addStretch();
    optionsRow->addWidget(rememberMeCheck_);
    layout->addLayout(optionsRow);

    layout->addSpacing(12);
}

void LoginDialog::setupServerFields(QVBoxLayout* layout, QWidget* parent) {
    // Server label row with saved connections button
    auto* serverLabelRow = new QHBoxLayout();
    serverLabelRow->setSpacing(4);

    auto* hostLabel = new QLabel("SERVER", parent);
    hostLabel->setStyleSheet(dialog_styles::field_label);
    serverLabelRow->addWidget(hostLabel);

    // Saved connections button (icon only)
    savedConnectionsButton_ = new QToolButton(parent);
    savedConnectionsButton_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_server_link_20_filled.svg", QColor("#909090")));
    savedConnectionsButton_->setIconSize(QSize(14, 14));
    savedConnectionsButton_->setStyleSheet(dialog_styles::saved_connections_button);
    savedConnectionsButton_->setToolTip(tr("Saved connections"));
    savedConnectionsButton_->setCursor(Qt::PointingHandCursor);
    savedConnectionsButton_->setPopupMode(QToolButton::InstantPopup);
    savedConnectionsButton_->setVisible(false); // Hidden until connections are set

    savedConnectionsMenu_ = new QMenu(savedConnectionsButton_);
    savedConnectionsButton_->setMenu(savedConnectionsMenu_);

    serverLabelRow->addWidget(savedConnectionsButton_);
    serverLabelRow->addStretch();
    layout->addLayout(serverLabelRow);
    layout->addSpacing(4);

    hostEdit_ = new QLineEdit(parent);
    hostEdit_->setPlaceholderText("localhost");
    hostEdit_->setText("localhost");
    hostEdit_->setStyleSheet(dialog_styles::input_field);
    hostEdit_->setFixedHeight(32);
    layout->addWidget(hostEdit_);

    layout->addSpacing(8);

    auto* portLabel = new QLabel("PORT", parent);
    portLabel->setStyleSheet(dialog_styles::field_label);
    layout->addWidget(portLabel);
    layout->addSpacing(4);

    portSpinBox_ = new QSpinBox(parent);
    portSpinBox_->setRange(1, 65535);
    portSpinBox_->setValue(55555);
    portSpinBox_->setStyleSheet(dialog_styles::spin_box);
    portSpinBox_->setFixedHeight(32);
    layout->addWidget(portSpinBox_);

    layout->addSpacing(12);
}

void LoginDialog::setupActions(QVBoxLayout* layout, QWidget* parent) {
    // Status label
    statusLabel_ = new QLabel(parent);
    statusLabel_->setStyleSheet(dialog_styles::status);
    statusLabel_->setAlignment(Qt::AlignCenter);
    layout->addWidget(statusLabel_);

    layout->addSpacing(4);

    // Login button
    loginButton_ = new QPushButton("Login", parent);
    loginButton_->setStyleSheet(dialog_styles::primary_button);
    loginButton_->setFixedHeight(40);
    loginButton_->setCursor(Qt::PointingHandCursor);
    connect(loginButton_, &QPushButton::clicked,
            this, &LoginDialog::onLoginClicked);
    layout->addWidget(loginButton_);

    layout->addSpacing(12);

    // Sign up row
    auto* signUpRow = new QHBoxLayout();
    signUpRow->setAlignment(Qt::AlignCenter);
    signUpLabel_ = new QLabel("Don't have an account?", parent);
    signUpLabel_->setStyleSheet("QLabel { background: transparent; color: #707070; font-size: 12px; }");

    signUpButton_ = new QPushButton("Register", parent);
    signUpButton_->setStyleSheet(dialog_styles::link_button);
    signUpButton_->setCursor(Qt::PointingHandCursor);
    connect(signUpButton_, &QPushButton::clicked,
            this, &LoginDialog::onSignUpClicked);

    signUpRow->addWidget(signUpLabel_);
    signUpRow->addWidget(signUpButton_);
    layout->addLayout(signUpRow);
}

void LoginDialog::setupFooter(QVBoxLayout* layout, QWidget* parent) {
    QString versionText = QString("v%1  %2")
        .arg(ORES_VERSION)
        .arg(QString::fromStdString(ORES_BUILD_INFO));
    auto* versionLabel = new QLabel(versionText, parent);
    versionLabel->setStyleSheet(dialog_styles::version);
    layout->addWidget(versionLabel, 0, Qt::AlignCenter);

    layout->addSpacing(4);

    auto* copyrightLabel = new QLabel(QString::fromUtf8("\u00A9 2025 ORE Studio"), parent);
    copyrightLabel->setStyleSheet(dialog_styles::version);
    layout->addWidget(copyrightLabel, 0, Qt::AlignCenter);

    layout->addSpacing(8);
}

void LoginDialog::onLoginClicked() {
    BOOST_LOG_SEV(lg(), trace) << "Login button clicked";

    if (!clientManager_) {
        MessageBoxHelper::critical(this, "Internal Error", "Client manager not initialized");
        return;
    }

    const auto username = usernameEdit_->text().trimmed();
    const auto password = passwordEdit_->text();
    const auto host = hostEdit_->text().trimmed();
    const auto port = static_cast<std::uint16_t>(portSpinBox_->value());

    // Validate input
    if (username.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a username.");
        usernameEdit_->setFocus();
        return;
    }

    if (password.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a password.");
        passwordEdit_->setFocus();
        return;
    }

    if (host.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a server host.");
        hostEdit_->setFocus();
        return;
    }

    // Disable form during connection
    enableForm(false);
    statusLabel_->setText("Connecting to server...");
    statusLabel_->setStyleSheet(dialog_styles::status);

    // Perform login asynchronously via ClientManager
    auto* watcher = new QFutureWatcher<LoginResult>(this);
    connect(watcher, &QFutureWatcher<LoginResult>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();
        onLoginResult(result);
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
    BOOST_LOG_SEV(lg(), debug) << "Login result received";

    if (result.success) {
        BOOST_LOG_SEV(lg(), debug) << "Login was successful";

        // Check if password reset is required
        if (result.password_reset_required) {
            BOOST_LOG_SEV(lg(), info) << "Password reset required";
            statusLabel_->setText("Password change required...");

            // Show change password dialog
            ChangePasswordDialog changeDialog(clientManager_, this);
            if (changeDialog.exec() == QDialog::Accepted) {
                BOOST_LOG_SEV(lg(), info) << "Password changed successfully";
                statusLabel_->setText("Login successful!");
                emit loginSucceeded(usernameEdit_->text().trimmed());
                emit closeRequested();
            } else {
                BOOST_LOG_SEV(lg(), warn) << "Password change canceled";
                // Disconnect since user canceled password change
                clientManager_->disconnect();
                enableForm(true);
                statusLabel_->setText("");
                MessageBoxHelper::warning(this, "Password Change Required",
                    "You must change your password to continue. Please login again to retry.");
            }
        } else {
            statusLabel_->setText("Login successful!");
            emit loginSucceeded(usernameEdit_->text().trimmed());
            emit closeRequested();
        }
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Login failed: " << result.error_message.toStdString();

        enableForm(true);
        statusLabel_->setText("");

        emit loginFailed(result.error_message);
        MessageBoxHelper::critical(this, "Login Failed",
            QString("Authentication failed: %1").arg(result.error_message));
    }
}

void LoginDialog::onSignUpClicked() {
    emit signUpRequested();
}

void LoginDialog::onGetStartedClicked() {
    usernameEdit_->setFocus();
}

void LoginDialog::onShowPasswordToggled(bool checked) {
    passwordEdit_->setEchoMode(checked ? QLineEdit::Normal : QLineEdit::Password);
}

}
