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
#include "ores.qt/ConnectionDetailDialog.hpp"
#include "ores.qt/TagSelectorWidget.hpp"
#include "ores.connections/service/connection_manager.hpp"
#include "ores.security/validation/password_validator.hpp"
#include <QtWidgets/QApplication>
#include <QFormLayout>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QMessageBox>
#include <QLabel>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

ConnectionDetailDialog::ConnectionDetailDialog(
    connections::service::connection_manager* manager,
    QWidget* parent)
    : QDialog(parent),
      manager_(manager) {

    setupUI();
}

ConnectionDetailDialog::~ConnectionDetailDialog() = default;

void ConnectionDetailDialog::setupUI() {
    setWindowTitle(tr("New Connection"));
    setMinimumWidth(450);

    auto* layout = new QVBoxLayout(this);

    auto* formLayout = new QFormLayout();

    nameEdit_ = new QLineEdit(this);
    nameEdit_->setPlaceholderText(tr("Enter a name for this connection"));
    formLayout->addRow(tr("Name:"), nameEdit_);

    folderCombo_ = new QComboBox(this);
    formLayout->addRow(tr("Folder:"), folderCombo_);

    // Tags
    tagSelector_ = new TagSelectorWidget(manager_, this);
    formLayout->addRow(tr("Tags:"), tagSelector_);

    formLayout->addRow(new QLabel(tr("<b>Connection Details</b>"), this), new QWidget(this));

    hostEdit_ = new QLineEdit(this);
    hostEdit_->setPlaceholderText(tr("e.g., localhost or 192.168.1.100"));
    formLayout->addRow(tr("Host:"), hostEdit_);

    portSpinBox_ = new QSpinBox(this);
    portSpinBox_->setRange(1, 65535);
    portSpinBox_->setValue(55555);
    formLayout->addRow(tr("Port:"), portSpinBox_);

    formLayout->addRow(new QLabel(tr("<b>Credentials</b>"), this), new QWidget(this));

    usernameEdit_ = new QLineEdit(this);
    usernameEdit_->setPlaceholderText(tr("Enter username"));
    formLayout->addRow(tr("Username:"), usernameEdit_);

    // Password with visibility toggle
    auto* passwordLayout = new QHBoxLayout();
    passwordEdit_ = new QLineEdit(this);
    passwordEdit_->setEchoMode(QLineEdit::Password);
    passwordEdit_->setPlaceholderText(tr("Enter password (optional)"));
    passwordLayout->addWidget(passwordEdit_);

    showPasswordCheckbox_ = new QCheckBox(tr("Show"), this);
    passwordLayout->addWidget(showPasswordCheckbox_);
    formLayout->addRow(tr("Password:"), passwordLayout);

    formLayout->addRow(new QLabel(tr("<b>Additional Info</b>"), this), new QWidget(this));

    descriptionEdit_ = new QTextEdit(this);
    descriptionEdit_->setPlaceholderText(tr("Optional notes about this connection"));
    descriptionEdit_->setMaximumHeight(80);
    formLayout->addRow(tr("Description:"), descriptionEdit_);

    layout->addLayout(formLayout);

    // Test button (initially hidden, shown when callback is set)
    testButton_ = new QPushButton(tr("Test Connection"), this);
    testButton_->setVisible(false);

    buttonBox_ = new QDialogButtonBox(
        QDialogButtonBox::Save | QDialogButtonBox::Cancel, this);
    buttonBox_->addButton(testButton_, QDialogButtonBox::ActionRole);
    layout->addWidget(buttonBox_);

    populateFolderCombo();

    // Connections
    connect(nameEdit_, &QLineEdit::textChanged,
            this, &ConnectionDetailDialog::updateSaveButtonState);
    connect(hostEdit_, &QLineEdit::textChanged,
            this, &ConnectionDetailDialog::updateSaveButtonState);
    connect(usernameEdit_, &QLineEdit::textChanged,
            this, &ConnectionDetailDialog::updateSaveButtonState);
    connect(passwordEdit_, &QLineEdit::textChanged,
            this, &ConnectionDetailDialog::onPasswordChanged);
    connect(showPasswordCheckbox_, &QCheckBox::toggled,
            this, &ConnectionDetailDialog::togglePasswordVisibility);
    connect(testButton_, &QPushButton::clicked,
            this, &ConnectionDetailDialog::onTestClicked);
    connect(buttonBox_, &QDialogButtonBox::accepted,
            this, &ConnectionDetailDialog::onSaveClicked);
    connect(buttonBox_, &QDialogButtonBox::rejected,
            this, &QDialog::reject);

    updateSaveButtonState();
}

void ConnectionDetailDialog::populateFolderCombo() {
    using namespace ores::logging;

    folderCombo_->clear();

    // Add "None" option for root level
    folderCombo_->addItem(tr("(No Folder)"), QString());

    try {
        auto folders = manager_->get_all_folders();
        for (const auto& folder : folders) {
            folderCombo_->addItem(
                QString::fromStdString(folder.name),
                QString::fromStdString(boost::uuids::to_string(folder.id)));
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load folders: " << e.what();
    }
}

void ConnectionDetailDialog::setEnvironment(
    const connections::domain::server_environment& env) {

    using namespace ores::logging;

    isAddMode_ = false;
    environmentId_ = env.id;
    passwordChanged_ = false;

    setWindowTitle(tr("Edit Connection"));

    nameEdit_->setText(QString::fromStdString(env.name));
    hostEdit_->setText(QString::fromStdString(env.host));
    portSpinBox_->setValue(env.port);
    usernameEdit_->setText(QString::fromStdString(env.username));
    descriptionEdit_->setPlainText(QString::fromStdString(env.description));

    // Password is not shown - user must re-enter if changing
    passwordEdit_->setPlaceholderText(tr("Enter new password to change"));

    // Set folder selection
    if (env.folder_id) {
        QString folderIdStr = QString::fromStdString(
            boost::uuids::to_string(*env.folder_id));
        int index = folderCombo_->findData(folderIdStr);
        if (index >= 0)
            folderCombo_->setCurrentIndex(index);
    } else {
        folderCombo_->setCurrentIndex(0); // No folder
    }

    BOOST_LOG_SEV(lg(), debug) << "Editing connection: " << env.name;
}

void ConnectionDetailDialog::setInitialFolder(
    const std::optional<boost::uuids::uuid>& folderId) {

    if (folderId) {
        QString folderIdStr = QString::fromStdString(
            boost::uuids::to_string(*folderId));
        int index = folderCombo_->findData(folderIdStr);
        if (index >= 0)
            folderCombo_->setCurrentIndex(index);
    }
}

connections::domain::server_environment ConnectionDetailDialog::getEnvironment() const {
    connections::domain::server_environment env;

    if (isAddMode_) {
        env.id = boost::uuids::random_generator()();
    } else {
        env.id = environmentId_;
    }

    env.name = nameEdit_->text().trimmed().toStdString();
    env.host = hostEdit_->text().trimmed().toStdString();
    env.port = portSpinBox_->value();
    env.username = usernameEdit_->text().trimmed().toStdString();
    env.description = descriptionEdit_->toPlainText().trimmed().toStdString();

    // Get folder ID from combo
    QString folderIdStr = folderCombo_->currentData().toString();
    if (!folderIdStr.isEmpty()) {
        boost::uuids::string_generator gen;
        env.folder_id = gen(folderIdStr.toStdString());
    }

    // encrypted_password is not set here - it's handled by connection_manager

    return env;
}

std::optional<std::string> ConnectionDetailDialog::getPassword() const {
    if (isAddMode_) {
        // Always return password for new connections
        return passwordEdit_->text().toStdString();
    }

    if (passwordChanged_) {
        return passwordEdit_->text().toStdString();
    }

    // Password wasn't changed - return nullopt to preserve existing
    return std::nullopt;
}

void ConnectionDetailDialog::onSaveClicked() {
    if (validateInput()) {
        accept();
    }
}

void ConnectionDetailDialog::updateSaveButtonState() {
    auto* saveButton = buttonBox_->button(QDialogButtonBox::Save);

    bool valid = !nameEdit_->text().trimmed().isEmpty() &&
                 !hostEdit_->text().trimmed().isEmpty() &&
                 !usernameEdit_->text().trimmed().isEmpty();

    saveButton->setEnabled(valid);
}

void ConnectionDetailDialog::onPasswordChanged() {
    passwordChanged_ = true;
}

void ConnectionDetailDialog::togglePasswordVisibility() {
    passwordEdit_->setEchoMode(
        showPasswordCheckbox_->isChecked() ? QLineEdit::Normal : QLineEdit::Password);
}

bool ConnectionDetailDialog::validateInput() {
    QString name = nameEdit_->text().trimmed();
    QString host = hostEdit_->text().trimmed();
    QString username = usernameEdit_->text().trimmed();

    if (name.isEmpty()) {
        QMessageBox::warning(this, tr("Validation Error"),
            tr("Connection name cannot be empty."));
        nameEdit_->setFocus();
        return false;
    }

    if (host.isEmpty()) {
        QMessageBox::warning(this, tr("Validation Error"),
            tr("Host cannot be empty."));
        hostEdit_->setFocus();
        return false;
    }

    if (username.isEmpty()) {
        QMessageBox::warning(this, tr("Validation Error"),
            tr("Username cannot be empty."));
        usernameEdit_->setFocus();
        return false;
    }

    // Validate password if provided (password is optional, but if set must meet policy)
    QString password = passwordEdit_->text();
    if (!password.isEmpty()) {
        // Only validate in add mode, or in edit mode if password was changed
        if (isAddMode_ || passwordChanged_) {
            auto result = security::validation::password_validator::validate(
                password.toStdString());
            if (!result.is_valid) {
                QMessageBox::warning(this, tr("Password Policy"),
                    tr("Password does not meet security requirements:\n\n%1")
                        .arg(QString::fromStdString(result.error_message)));
                passwordEdit_->setFocus();
                return false;
            }
        }
    }

    return true;
}

void ConnectionDetailDialog::setTags(const std::vector<connections::domain::tag>& tags) {
    tagSelector_->setSelectedTags(tags);
}

std::vector<boost::uuids::uuid> ConnectionDetailDialog::getSelectedTagIds() const {
    return tagSelector_->selectedTagIds();
}

void ConnectionDetailDialog::setTestCallback(TestConnectionCallback callback) {
    testCallback_ = std::move(callback);
    testButton_->setVisible(static_cast<bool>(testCallback_));
}

void ConnectionDetailDialog::onTestClicked() {
    using namespace ores::logging;

    if (!testCallback_) {
        return;
    }

    QString host = hostEdit_->text().trimmed();
    QString username = usernameEdit_->text().trimmed();
    QString password = passwordEdit_->text();
    int port = portSpinBox_->value();

    if (host.isEmpty() || username.isEmpty() || password.isEmpty()) {
        QMessageBox::warning(this, tr("Test Connection"),
            tr("Please enter host, username, and password to test the connection."));
        return;
    }

    // Disable test button during test
    testButton_->setEnabled(false);
    testButton_->setText(tr("Testing..."));
    QApplication::processEvents();

    BOOST_LOG_SEV(lg(), info) << "Testing connection to " << host.toStdString()
                              << ":" << port;

    QString error = testCallback_(host, port, username, password);

    testButton_->setEnabled(true);
    testButton_->setText(tr("Test Connection"));

    if (error.isEmpty()) {
        QMessageBox::information(this, tr("Test Connection"),
            tr("Connection successful!"));
        BOOST_LOG_SEV(lg(), info) << "Connection test successful";
    } else {
        QMessageBox::warning(this, tr("Test Connection"),
            tr("Connection failed: %1").arg(error));
        BOOST_LOG_SEV(lg(), warn) << "Connection test failed: " << error.toStdString();
    }
}

}
