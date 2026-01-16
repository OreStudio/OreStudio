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
#include "ores.qt/SignUpDialog.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.utility/version/version.hpp"
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QKeyEvent>
#include <QRegularExpression>
#include <QtConcurrent>
#include <QFutureWatcher>

namespace ores::qt {

using namespace ores::logging;

namespace {

const QString panelStyle = R"(
    QWidget#mainPanel {
        background-color: #1A1A1A;
    }
)";

const QString titleStyle = R"(
    QLabel {
        background: transparent;
        color: #ffffff;
        font-size: 28px;
        font-weight: bold;
        letter-spacing: 2px;
    }
)";

const QString subtitleStyle = R"(
    QLabel {
        background: transparent;
        color: #707070;
        font-size: 12px;
    }
)";

const QString inputFieldStyle = R"(
    QLineEdit {
        background-color: #2d2d2d;
        border: 1px solid #3d3d3d;
        border-radius: 4px;
        padding: 8px 12px;
        font-size: 13px;
        color: #ffffff;
    }
    QLineEdit:focus {
        border-color: #5a5a5a;
        background-color: #333333;
    }
    QLineEdit::placeholder {
        color: #707070;
    }
)";

const QString inputFieldMatchStyle = R"(
    QLineEdit {
        background-color: #2d2d2d;
        border: 2px solid #4CAF50;
        border-radius: 4px;
        padding: 8px 12px;
        font-size: 13px;
        color: #ffffff;
    }
    QLineEdit:focus {
        border-color: #4CAF50;
        background-color: #333333;
    }
)";

const QString inputFieldMismatchStyle = R"(
    QLineEdit {
        background-color: #2d2d2d;
        border: 2px solid #FF9800;
        border-radius: 4px;
        padding: 8px 12px;
        font-size: 13px;
        color: #ffffff;
    }
    QLineEdit:focus {
        border-color: #FF9800;
        background-color: #333333;
    }
)";

const QString spinBoxStyle = R"(
    QSpinBox {
        background-color: #2d2d2d;
        border: 1px solid #3d3d3d;
        border-radius: 4px;
        padding: 8px 12px;
        font-size: 13px;
        color: #ffffff;
    }
    QSpinBox:focus {
        border-color: #5a5a5a;
        background-color: #333333;
    }
)";

const QString signUpButtonStyle = R"(
    QPushButton {
        background-color: #3d3d3d;
        color: #ffffff;
        border: none;
        border-radius: 4px;
        padding: 10px 24px;
        font-size: 14px;
        font-weight: bold;
    }
    QPushButton:hover {
        background-color: #4a4a4a;
    }
    QPushButton:pressed {
        background-color: #333333;
    }
    QPushButton:disabled {
        background-color: #2a2a2a;
        color: #555555;
    }
)";

const QString checkboxStyle = R"(
    QCheckBox {
        background: transparent;
        color: #909090;
        font-size: 12px;
        spacing: 6px;
    }
    QCheckBox::indicator {
        width: 14px;
        height: 14px;
        border: 1px solid #3d3d3d;
        border-radius: 2px;
        background-color: #2d2d2d;
    }
    QCheckBox::indicator:checked {
        background-color: #4a4a4a;
        border-color: #5a5a5a;
    }
)";

const QString linkButtonStyle = R"(
    QPushButton {
        background: transparent;
        border: none;
        color: #909090;
        font-size: 12px;
        padding: 0;
    }
    QPushButton:hover {
        color: #ffffff;
        text-decoration: underline;
    }
)";

const QString fieldLabelStyle = R"(
    QLabel {
        background: transparent;
        color: #909090;
        font-size: 10px;
        font-weight: bold;
        letter-spacing: 1px;
    }
)";

const QString versionStyle = R"(
    QLabel {
        background: transparent;
        color: #505050;
        font-size: 9px;
    }
)";

const QString statusStyle = R"(
    QLabel {
        background: transparent;
        color: #707070;
        font-size: 11px;
        font-style: italic;
    }
)";

}

SignUpDialog::SignUpDialog(QWidget* parent)
    : QWidget(parent) {
    setupUI();

    // Register result types for cross-thread signal/slot
    qRegisterMetaType<SignupResult>("SignupResult");
    qRegisterMetaType<LoginResult>("LoginResult");
}

SignUpDialog::~SignUpDialog() = default;

QSize SignUpDialog::sizeHint() const {
    return {400, 620};
}

void SignUpDialog::keyPressEvent(QKeyEvent* event) {
    if (event->key() == Qt::Key_Escape) {
        emit closeRequested();
    } else {
        QWidget::keyPressEvent(event);
    }
}

void SignUpDialog::setServer(const QString& server) {
    hostEdit_->setText(server);
}

void SignUpDialog::setPort(int port) {
    portSpinBox_->setValue(port);
}

QString SignUpDialog::getServer() const {
    return hostEdit_->text().trimmed();
}

int SignUpDialog::getPort() const {
    return portSpinBox_->value();
}

void SignUpDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

QString SignUpDialog::getRegisteredUsername() const {
    return registeredUsername_;
}

void SignUpDialog::enableForm(bool enabled) {
    usernameEdit_->setEnabled(enabled);
    emailEdit_->setEnabled(enabled);
    passwordEdit_->setEnabled(enabled);
    confirmPasswordEdit_->setEnabled(enabled);
    hostEdit_->setEnabled(enabled);
    portSpinBox_->setEnabled(enabled);
    signUpButton_->setEnabled(enabled);
    loginButton_->setEnabled(enabled);
}

bool SignUpDialog::validateInput() {
    const auto username = usernameEdit_->text().trimmed();
    const auto email = emailEdit_->text().trimmed();
    const auto password = passwordEdit_->text();
    const auto confirmPassword = confirmPasswordEdit_->text();
    const auto host = hostEdit_->text().trimmed();

    if (username.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a username.");
        usernameEdit_->setFocus();
        return false;
    }

    if (email.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter an email address.");
        emailEdit_->setFocus();
        return false;
    }

    const QRegularExpression emailRegex(R"(.+@.+\..+)");
    if (!emailRegex.match(email).hasMatch()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please enter a valid email address.");
        emailEdit_->setFocus();
        return false;
    }

    if (password.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a password.");
        passwordEdit_->setFocus();
        return false;
    }

    if (password.length() < 12) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Password must be at least 12 characters long.");
        passwordEdit_->setFocus();
        return false;
    }

    if (password != confirmPassword) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Passwords do not match. Please try again.");
        confirmPasswordEdit_->setFocus();
        confirmPasswordEdit_->selectAll();
        return false;
    }

    if (host.isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please enter a server host.");
        hostEdit_->setFocus();
        return false;
    }

    return true;
}

void SignUpDialog::setupUI() {
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);
    mainLayout->setSpacing(0);

    // Main panel
    auto* mainPanel = new QWidget(this);
    mainPanel->setObjectName("mainPanel");
    mainPanel->setStyleSheet(panelStyle);
    setupPanel(mainPanel);

    mainLayout->addWidget(mainPanel);
}

void SignUpDialog::setupPanel(QWidget* parent) {
    auto* layout = new QVBoxLayout(parent);
    layout->setContentsMargins(36, 20, 36, 16);
    layout->setSpacing(0);

    layout->addStretch(1);

    // Title - CREATE ACCOUNT
    titleLabel_ = new QLabel("CREATE ACCOUNT", parent);
    titleLabel_->setStyleSheet(titleStyle);
    layout->addWidget(titleLabel_, 0, Qt::AlignCenter);

    layout->addSpacing(4);

    // Subtitle
    auto* subtitleLabel = new QLabel("Join ORE Studio", parent);
    subtitleLabel->setStyleSheet(subtitleStyle);
    layout->addWidget(subtitleLabel, 0, Qt::AlignCenter);

    layout->addSpacing(20);

    // Username field
    auto* usernameLabel = new QLabel("USERNAME", parent);
    usernameLabel->setStyleSheet(fieldLabelStyle);
    layout->addWidget(usernameLabel);
    layout->addSpacing(4);

    usernameEdit_ = new QLineEdit(parent);
    usernameEdit_->setPlaceholderText("Choose a username");
    usernameEdit_->setStyleSheet(inputFieldStyle);
    usernameEdit_->setFixedHeight(36);
    layout->addWidget(usernameEdit_);

    layout->addSpacing(10);

    // Email field
    auto* emailLabel = new QLabel("EMAIL", parent);
    emailLabel->setStyleSheet(fieldLabelStyle);
    layout->addWidget(emailLabel);
    layout->addSpacing(4);

    emailEdit_ = new QLineEdit(parent);
    emailEdit_->setPlaceholderText("Enter your email address");
    emailEdit_->setStyleSheet(inputFieldStyle);
    emailEdit_->setFixedHeight(36);
    layout->addWidget(emailEdit_);

    layout->addSpacing(10);

    // Password field
    auto* passwordLabel = new QLabel("PASSWORD", parent);
    passwordLabel->setStyleSheet(fieldLabelStyle);
    layout->addWidget(passwordLabel);
    layout->addSpacing(4);

    passwordEdit_ = new QLineEdit(parent);
    passwordEdit_->setPlaceholderText("Choose a password (min 12 chars)");
    passwordEdit_->setEchoMode(QLineEdit::Password);
    passwordEdit_->setStyleSheet(inputFieldStyle);
    passwordEdit_->setFixedHeight(36);
    layout->addWidget(passwordEdit_);

    layout->addSpacing(10);

    // Confirm password field
    auto* confirmLabel = new QLabel("CONFIRM PASSWORD", parent);
    confirmLabel->setStyleSheet(fieldLabelStyle);
    layout->addWidget(confirmLabel);
    layout->addSpacing(4);

    confirmPasswordEdit_ = new QLineEdit(parent);
    confirmPasswordEdit_->setPlaceholderText("Confirm your password");
    confirmPasswordEdit_->setEchoMode(QLineEdit::Password);
    confirmPasswordEdit_->setStyleSheet(inputFieldStyle);
    confirmPasswordEdit_->setFixedHeight(36);
    layout->addWidget(confirmPasswordEdit_);

    // Connect password fields for match indicator
    connect(passwordEdit_, &QLineEdit::textChanged,
            this, &SignUpDialog::updatePasswordMatchIndicator);
    connect(confirmPasswordEdit_, &QLineEdit::textChanged,
            this, &SignUpDialog::updatePasswordMatchIndicator);

    // Show password checkbox
    layout->addSpacing(8);
    showPasswordCheck_ = new QCheckBox("Show passwords", parent);
    showPasswordCheck_->setStyleSheet(checkboxStyle);
    connect(showPasswordCheck_, &QCheckBox::toggled,
            this, &SignUpDialog::onShowPasswordToggled);
    layout->addWidget(showPasswordCheck_);

    layout->addSpacing(10);

    // Server field
    auto* hostLabel = new QLabel("SERVER", parent);
    hostLabel->setStyleSheet(fieldLabelStyle);
    layout->addWidget(hostLabel);
    layout->addSpacing(4);

    hostEdit_ = new QLineEdit(parent);
    hostEdit_->setPlaceholderText("localhost");
    hostEdit_->setText("localhost");
    hostEdit_->setStyleSheet(inputFieldStyle);
    hostEdit_->setFixedHeight(32);
    layout->addWidget(hostEdit_);

    layout->addSpacing(8);

    auto* portLabel = new QLabel("PORT", parent);
    portLabel->setStyleSheet(fieldLabelStyle);
    layout->addWidget(portLabel);
    layout->addSpacing(4);

    portSpinBox_ = new QSpinBox(parent);
    portSpinBox_->setRange(1, 65535);
    portSpinBox_->setValue(55555);
    portSpinBox_->setStyleSheet(spinBoxStyle);
    portSpinBox_->setFixedHeight(32);
    layout->addWidget(portSpinBox_);

    layout->addSpacing(10);

    // Status label
    statusLabel_ = new QLabel(parent);
    statusLabel_->setStyleSheet(statusStyle);
    statusLabel_->setAlignment(Qt::AlignCenter);
    layout->addWidget(statusLabel_);

    layout->addSpacing(4);

    // Sign up button
    signUpButton_ = new QPushButton("Create Account", parent);
    signUpButton_->setStyleSheet(signUpButtonStyle);
    signUpButton_->setFixedHeight(40);
    signUpButton_->setCursor(Qt::PointingHandCursor);
    connect(signUpButton_, &QPushButton::clicked,
            this, &SignUpDialog::onSignUpClicked);
    layout->addWidget(signUpButton_);

    layout->addSpacing(12);

    // Login row
    auto* loginRow = new QHBoxLayout();
    loginRow->setAlignment(Qt::AlignCenter);
    loginLabel_ = new QLabel("Already have an account?", parent);
    loginLabel_->setStyleSheet("QLabel { background: transparent; color: #707070; font-size: 12px; }");

    loginButton_ = new QPushButton("Log in", parent);
    loginButton_->setStyleSheet(linkButtonStyle);
    loginButton_->setCursor(Qt::PointingHandCursor);
    connect(loginButton_, &QPushButton::clicked,
            this, &SignUpDialog::onLoginClicked);

    loginRow->addWidget(loginLabel_);
    loginRow->addWidget(loginButton_);
    layout->addLayout(loginRow);

    layout->addStretch(1);

    // Version info at bottom
    QString versionText = QString("v%1  %2")
        .arg(ORES_VERSION)
        .arg(QString::fromStdString(ORES_BUILD_INFO));
    auto* versionLabel = new QLabel(versionText, parent);
    versionLabel->setStyleSheet(versionStyle);
    layout->addWidget(versionLabel, 0, Qt::AlignCenter);

    layout->addSpacing(4);

    auto* copyrightLabel = new QLabel(QString::fromUtf8("\u00A9 2025 ORE Studio"), parent);
    copyrightLabel->setStyleSheet(versionStyle);
    layout->addWidget(copyrightLabel, 0, Qt::AlignCenter);

    layout->addSpacing(8);
}

void SignUpDialog::onSignUpClicked() {
    BOOST_LOG_SEV(lg(), trace) << "Sign up button clicked";

    if (!clientManager_) {
        MessageBoxHelper::critical(this, "Internal Error", "Client manager not initialized");
        return;
    }

    if (!validateInput()) {
        return;
    }

    const auto username = usernameEdit_->text().trimmed();
    const auto email = emailEdit_->text().trimmed();
    const auto password = passwordEdit_->text();
    const auto host = hostEdit_->text().trimmed();
    const auto port = static_cast<std::uint16_t>(portSpinBox_->value());

    // Disable form during registration
    enableForm(false);
    statusLabel_->setText("Creating account...");
    statusLabel_->setStyleSheet(statusStyle);

    // Perform signup asynchronously via ClientManager
    auto* watcher = new QFutureWatcher<SignupResult>(this);
    connect(watcher, &QFutureWatcher<SignupResult>::finished,
            [this, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();
        onSignUpResult(result);
    });

    QFuture<SignupResult> future = QtConcurrent::run(
        [this, host, port, username, email, password]() -> SignupResult {
            return clientManager_->signup(
                host.toStdString(), port, username.toStdString(),
                email.toStdString(), password.toStdString());
        }
    );

    watcher->setFuture(future);
}

void SignUpDialog::onSignUpResult(const SignupResult& result) {
    BOOST_LOG_SEV(lg(), debug) << "Signup result received";

    if (result.success) {
        BOOST_LOG_SEV(lg(), info) << "Signup successful for user: "
                                  << result.username.toStdString();

        registeredUsername_ = result.username;
        emit signupSucceeded(result.username);

        // Auto-login with the credentials we just used
        statusLabel_->setText("Account created! Logging in...");
        statusLabel_->setStyleSheet("QLabel { background: transparent; color: #4CAF50; font-size: 11px; }");

        const auto username = usernameEdit_->text().trimmed();
        const auto password = passwordEdit_->text();
        const auto host = hostEdit_->text().trimmed();
        const auto port = static_cast<std::uint16_t>(portSpinBox_->value());

        auto* watcher = new QFutureWatcher<LoginResult>(this);
        connect(watcher, &QFutureWatcher<LoginResult>::finished,
                [this, watcher]() {
            const auto loginResult = watcher->result();
            watcher->deleteLater();
            onLoginResult(loginResult);
        });

        QFuture<LoginResult> future = QtConcurrent::run(
            [this, host, port, username, password]() -> LoginResult {
                return clientManager_->connectAndLogin(
                    host.toStdString(), port, username.toStdString(), password.toStdString());
            }
        );

        watcher->setFuture(future);
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Signup failed: "
                                  << result.error_message.toStdString();

        enableForm(true);
        statusLabel_->setText("");

        emit signupFailed(result.error_message);
        MessageBoxHelper::critical(this, "Sign Up Failed",
            QString("Account creation failed: %1").arg(result.error_message));
    }
}

void SignUpDialog::onLoginResult(const LoginResult& result) {
    BOOST_LOG_SEV(lg(), debug) << "Auto-login result received";

    if (result.success) {
        BOOST_LOG_SEV(lg(), info) << "Auto-login successful for user: "
                                  << registeredUsername_.toStdString();

        statusLabel_->setText("Login successful!");
        emit loginSucceeded(registeredUsername_);
        emit closeRequested();
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Auto-login failed: "
                                  << result.error_message.toStdString();

        // Auto-login failed, but signup succeeded - let user know they can login manually
        enableForm(true);
        statusLabel_->setText("");

        MessageBoxHelper::warning(this, "Auto-Login Failed",
            QString("Your account was created successfully, but automatic login failed: %1\n\n"
                    "Please close this dialog and log in manually.")
                .arg(result.error_message));
    }
}

void SignUpDialog::onLoginClicked() {
    emit loginRequested();
}

void SignUpDialog::onShowPasswordToggled(bool checked) {
    const auto mode = checked ? QLineEdit::Normal : QLineEdit::Password;
    passwordEdit_->setEchoMode(mode);
    confirmPasswordEdit_->setEchoMode(mode);
}

void SignUpDialog::updatePasswordMatchIndicator() {
    const QString password = passwordEdit_->text();
    const QString confirmPassword = confirmPasswordEdit_->text();

    // Only show indicator when confirm field has content
    if (confirmPassword.isEmpty()) {
        confirmPasswordEdit_->setStyleSheet(inputFieldStyle);
        return;
    }

    if (password == confirmPassword) {
        confirmPasswordEdit_->setStyleSheet(inputFieldMatchStyle);
    } else {
        confirmPasswordEdit_->setStyleSheet(inputFieldMismatchStyle);
    }
}

}
