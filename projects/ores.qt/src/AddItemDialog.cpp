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
#include "ores.qt/AddItemDialog.hpp"
#include "ores.qt/TagSelectorWidget.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.connections/service/connection_manager.hpp"
#include "ores.security/validation/password_validator.hpp"
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

AddItemDialog::AddItemDialog(
    connections::service::connection_manager* manager,
    QWidget* parent)
    : QWidget(parent),
      manager_(manager) {

    setupUI();
    updateFieldVisibility();
    updateSaveButtonState();
}

AddItemDialog::~AddItemDialog() {
    // Cancel any pending async operations
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
    }
}

void AddItemDialog::setupUI() {
    WidgetUtils::setupComboBoxes(this);
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);
    mainLayout->setSpacing(0);

    setupToolbar();
    mainLayout->addWidget(toolBar_);

    // Content area with margins
    auto* contentWidget = new QWidget(this);
    auto* contentLayout = new QVBoxLayout(contentWidget);
    contentLayout->setContentsMargins(16, 16, 16, 16);

    auto* formLayout = new QFormLayout();
    formLayout->setSpacing(8);

    // Type selector
    typeCombo_ = new QComboBox(this);
    typeCombo_->addItem(tr("Folder"), static_cast<int>(ItemType::Folder));
    typeCombo_->addItem(tr("Environment"), static_cast<int>(ItemType::Environment));
    typeCombo_->addItem(tr("Connection"), static_cast<int>(ItemType::Connection));
    typeCombo_->setCurrentIndex(2); // Default to Connection
    formLayout->addRow(tr("Type:"), typeCombo_);

    // Common fields
    nameEdit_ = new QLineEdit(this);
    nameEdit_->setPlaceholderText(tr("Enter a name"));
    formLayout->addRow(tr("Name:"), nameEdit_);

    folderCombo_ = new QComboBox(this);
    formLayout->addRow(tr("Folder:"), folderCombo_);

    descriptionEdit_ = new QTextEdit(this);
    descriptionEdit_->setPlaceholderText(tr("Optional description or notes"));
    descriptionEdit_->setMaximumHeight(80);
    formLayout->addRow(tr("Description:"), descriptionEdit_);

    // Connection-only: link to environment
    environmentLabel_ = new QLabel(tr("Environment:"), this);
    environmentCombo_ = new QComboBox(this);
    formLayout->addRow(environmentLabel_, environmentCombo_);

    // Environment and Connection fields
    hostLabel_ = new QLabel(tr("Host:"), this);
    hostEdit_ = new QLineEdit(this);
    hostEdit_->setPlaceholderText(tr("e.g., localhost or 192.168.1.100"));
    formLayout->addRow(hostLabel_, hostEdit_);

    portLabel_ = new QLabel(tr("Port:"), this);
    portSpinBox_ = new QSpinBox(this);
    portSpinBox_->setRange(1, 65535);
    portSpinBox_->setValue(55555);
    formLayout->addRow(portLabel_, portSpinBox_);

    // Connection-only: credentials
    usernameLabel_ = new QLabel(tr("Username:"), this);
    usernameEdit_ = new QLineEdit(this);
    usernameEdit_->setPlaceholderText(tr("Enter username"));
    formLayout->addRow(usernameLabel_, usernameEdit_);

    // Password with visibility toggle
    passwordLabel_ = new QLabel(tr("Password:"), this);
    passwordWidget_ = new QWidget(this);
    auto* passwordLayout = new QHBoxLayout(passwordWidget_);
    passwordLayout->setContentsMargins(0, 0, 0, 0);
    passwordEdit_ = new QLineEdit(this);
    passwordEdit_->setEchoMode(QLineEdit::Password);
    passwordEdit_->setPlaceholderText(tr("Enter password (optional)"));
    passwordLayout->addWidget(passwordEdit_);
    showPasswordCheckbox_ = new QCheckBox(tr("Show"), this);
    passwordLayout->addWidget(showPasswordCheckbox_);
    formLayout->addRow(passwordLabel_, passwordWidget_);

    // Tags (environment and connection)
    tagLabel_ = new QLabel(tr("Tags:"), this);
    tagSelector_ = new TagSelectorWidget(manager_, this);
    formLayout->addRow(tagLabel_, tagSelector_);

    contentLayout->addLayout(formLayout);
    contentLayout->addStretch();

    mainLayout->addWidget(contentWidget);

    populateFolderCombo();
    populateEnvironmentCombo();

    // Connections
    connect(typeCombo_, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &AddItemDialog::onTypeChanged);
    connect(nameEdit_, &QLineEdit::textChanged,
            this, &AddItemDialog::updateSaveButtonState);
    connect(hostEdit_, &QLineEdit::textChanged,
            this, &AddItemDialog::updateSaveButtonState);
    connect(usernameEdit_, &QLineEdit::textChanged,
            this, &AddItemDialog::updateSaveButtonState);
    connect(passwordEdit_, &QLineEdit::textChanged,
            this, &AddItemDialog::onPasswordChanged);
    connect(showPasswordCheckbox_, &QCheckBox::toggled,
            this, &AddItemDialog::togglePasswordVisibility);
    connect(environmentCombo_, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, &AddItemDialog::onEnvironmentComboChanged);
}

void AddItemDialog::setupToolbar() {
    toolBar_ = new QToolBar(this);
    toolBar_->setIconSize(QSize(20, 20));

    saveAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor),
        tr("Save"));
    saveAction_->setToolTip(tr("Save changes"));

    toolBar_->addSeparator();

    testAction_ = toolBar_->addAction(
        IconUtils::createRecoloredIcon(Icon::PlugConnected, IconUtils::DefaultIconColor),
        tr("Test"));
    testAction_->setToolTip(tr("Test connection"));
    testAction_->setVisible(false);

    connect(saveAction_, &QAction::triggered, this, &AddItemDialog::onSaveClicked);
    connect(testAction_, &QAction::triggered, this, &AddItemDialog::onTestClicked);
}

void AddItemDialog::populateFolderCombo() {
    using namespace ores::logging;

    folderCombo_->clear();
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

void AddItemDialog::populateEnvironmentCombo() {
    using namespace ores::logging;

    environmentCombo_->clear();
    environmentCombo_->addItem(tr("— manual entry —"), QString());

    try {
        auto envs = manager_->get_all_environments();
        for (const auto& env : envs) {
            environmentCombo_->addItem(
                QString::fromStdString(env.name),
                QString::fromStdString(boost::uuids::to_string(env.id)));
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to load environments: " << e.what();
    }
}

void AddItemDialog::setItemType(ItemType type) {
    itemType_ = type;
    int idx = 0;
    if (type == ItemType::Environment) idx = 1;
    else if (type == ItemType::Connection) idx = 2;
    typeCombo_->setCurrentIndex(idx);
    updateFieldVisibility();
    updateSaveButtonState();
}

void AddItemDialog::setCreateMode(bool createMode) {
    isCreateMode_ = createMode;
    typeCombo_->setEnabled(createMode); // Disable type change in edit mode

    if (!createMode) {
        QString typeName;
        if (itemType_ == ItemType::Folder) typeName = tr("Folder");
        else if (itemType_ == ItemType::Environment) typeName = tr("Environment");
        else typeName = tr("Connection");
        setWindowTitle(tr("Edit %1").arg(typeName));
    } else {
        setWindowTitle(tr("Add Item"));
    }
}

void AddItemDialog::onTypeChanged(int index) {
    itemType_ = static_cast<ItemType>(typeCombo_->itemData(index).toInt());
    updateFieldVisibility();
    updateSaveButtonState();
}

void AddItemDialog::onEnvironmentComboChanged(int index) {
    if (index <= 0) {
        // Manual entry — unlock host/port
        linkedEnvironmentId_ = std::nullopt;
        hostEdit_->setReadOnly(false);
        portSpinBox_->setEnabled(true);
        hostEdit_->setPlaceholderText(tr("e.g., localhost or 192.168.1.100"));
    } else {
        // Environment selected — fill and lock host/port
        QString envIdStr = environmentCombo_->itemData(index).toString();
        if (envIdStr.isEmpty()) return;

        try {
            boost::uuids::string_generator gen;
            auto envId = gen(envIdStr.toStdString());
            linkedEnvironmentId_ = envId;

            auto env = manager_->get_environment(envId);
            if (env) {
                hostEdit_->setText(QString::fromStdString(env->host));
                portSpinBox_->setValue(env->port);
                hostEdit_->setReadOnly(true);
                portSpinBox_->setEnabled(false);
            }
        } catch (const std::exception& e) {
            using namespace ores::logging;
            BOOST_LOG_SEV(lg(), error) << "Failed to load environment: " << e.what();
        }
    }
    updateSaveButtonState();
}

void AddItemDialog::updateFieldVisibility() {
    bool isEnvironment = (itemType_ == ItemType::Environment);
    bool isConnection = (itemType_ == ItemType::Connection);

    // Environment combo: Connection only
    environmentLabel_->setVisible(isConnection);
    environmentCombo_->setVisible(isConnection);

    // Host/port: Environment and Connection
    hostLabel_->setVisible(isEnvironment || isConnection);
    hostEdit_->setVisible(isEnvironment || isConnection);
    hostEdit_->setEnabled(isEnvironment || isConnection);

    portLabel_->setVisible(isEnvironment || isConnection);
    portSpinBox_->setVisible(isEnvironment || isConnection);
    portSpinBox_->setEnabled(isEnvironment || isConnection);

    // Credentials: Connection only
    usernameLabel_->setVisible(isConnection);
    usernameEdit_->setVisible(isConnection);
    usernameEdit_->setEnabled(isConnection);

    passwordLabel_->setVisible(isConnection);
    passwordWidget_->setVisible(isConnection);
    passwordEdit_->setEnabled(isConnection);
    showPasswordCheckbox_->setEnabled(isConnection);

    // Tags: Environment and Connection
    tagLabel_->setVisible(isEnvironment || isConnection);
    tagSelector_->setVisible(isEnvironment || isConnection);
    tagSelector_->setEnabled(isEnvironment || isConnection);

    // Test button only for connections with callback
    testAction_->setVisible(isConnection && static_cast<bool>(testCallback_));

    // Reset host/port lock state when switching away from Connection
    if (!isConnection) {
        hostEdit_->setReadOnly(false);
        portSpinBox_->setEnabled(isEnvironment || isConnection);
    }
}

void AddItemDialog::updateSaveButtonState() {
    bool valid = !nameEdit_->text().trimmed().isEmpty();

    if (itemType_ == ItemType::Environment) {
        valid = valid && !hostEdit_->text().trimmed().isEmpty();
    } else if (itemType_ == ItemType::Connection) {
        bool hasHost = !hostEdit_->text().trimmed().isEmpty() || linkedEnvironmentId_.has_value();
        valid = valid && hasHost && !usernameEdit_->text().trimmed().isEmpty();
    }

    saveAction_->setEnabled(valid);
}

void AddItemDialog::setFolder(const connections::domain::folder& folder) {
    isCreateMode_ = false;
    folderId_ = folder.id;
    itemType_ = ItemType::Folder;

    nameEdit_->setText(QString::fromStdString(folder.name));
    descriptionEdit_->setPlainText(QString::fromStdString(folder.description));

    // Set parent folder
    if (folder.parent_id) {
        QString parentIdStr = QString::fromStdString(
            boost::uuids::to_string(*folder.parent_id));
        int index = folderCombo_->findData(parentIdStr);
        if (index >= 0)
            folderCombo_->setCurrentIndex(index);
    } else {
        folderCombo_->setCurrentIndex(0);
    }

    setItemType(ItemType::Folder);
    setCreateMode(false);
}

connections::domain::folder AddItemDialog::getFolder() const {
    connections::domain::folder folder;

    if (isCreateMode_) {
        folder.id = boost::uuids::random_generator()();
    } else {
        folder.id = folderId_;
    }

    folder.name = nameEdit_->text().trimmed().toStdString();
    folder.description = descriptionEdit_->toPlainText().trimmed().toStdString();

    QString folderIdStr = folderCombo_->currentData().toString();
    if (!folderIdStr.isEmpty()) {
        boost::uuids::string_generator gen;
        folder.parent_id = gen(folderIdStr.toStdString());
    }

    return folder;
}

void AddItemDialog::setInitialParent(const std::optional<boost::uuids::uuid>& parentId) {
    if (parentId) {
        QString parentIdStr = QString::fromStdString(
            boost::uuids::to_string(*parentId));
        int index = folderCombo_->findData(parentIdStr);
        if (index >= 0)
            folderCombo_->setCurrentIndex(index);
    }
}

void AddItemDialog::setEnvironment(const connections::domain::environment& env) {
    isCreateMode_ = false;
    pureEnvironmentId_ = env.id;
    itemType_ = ItemType::Environment;

    nameEdit_->setText(QString::fromStdString(env.name));
    hostEdit_->setText(QString::fromStdString(env.host));
    portSpinBox_->setValue(env.port);
    descriptionEdit_->setPlainText(QString::fromStdString(env.description));

    // Set folder
    if (env.folder_id) {
        QString folderIdStr = QString::fromStdString(
            boost::uuids::to_string(*env.folder_id));
        int index = folderCombo_->findData(folderIdStr);
        if (index >= 0)
            folderCombo_->setCurrentIndex(index);
    } else {
        folderCombo_->setCurrentIndex(0);
    }

    setItemType(ItemType::Environment);
    setCreateMode(false);
}

connections::domain::environment AddItemDialog::getEnvironment() const {
    connections::domain::environment env;

    if (isCreateMode_) {
        env.id = boost::uuids::random_generator()();
    } else {
        env.id = pureEnvironmentId_;
    }

    env.name = nameEdit_->text().trimmed().toStdString();
    env.host = hostEdit_->text().trimmed().toStdString();
    env.port = portSpinBox_->value();
    env.description = descriptionEdit_->toPlainText().trimmed().toStdString();

    QString folderIdStr = folderCombo_->currentData().toString();
    if (!folderIdStr.isEmpty()) {
        boost::uuids::string_generator gen;
        env.folder_id = gen(folderIdStr.toStdString());
    }

    return env;
}

void AddItemDialog::setConnection(const connections::domain::connection& conn) {
    isCreateMode_ = false;
    connectionId_ = conn.id;
    itemType_ = ItemType::Connection;
    passwordChanged_ = false;

    nameEdit_->setText(QString::fromStdString(conn.name));
    usernameEdit_->setText(QString::fromStdString(conn.username));
    descriptionEdit_->setPlainText(QString::fromStdString(conn.description));
    passwordEdit_->setPlaceholderText(tr("Enter new password to change"));

    // Set up environment link or manual host/port
    if (conn.environment_id) {
        linkedEnvironmentId_ = conn.environment_id;
        QString envIdStr = QString::fromStdString(
            boost::uuids::to_string(*conn.environment_id));
        int index = environmentCombo_->findData(envIdStr);
        if (index >= 0) {
            environmentCombo_->setCurrentIndex(index);
            // onEnvironmentComboChanged will fill and lock host/port
        }
    } else {
        linkedEnvironmentId_ = std::nullopt;
        environmentCombo_->setCurrentIndex(0);
        if (conn.host) hostEdit_->setText(QString::fromStdString(*conn.host));
        if (conn.port) portSpinBox_->setValue(*conn.port);
    }

    // Set folder
    if (conn.folder_id) {
        QString folderIdStr = QString::fromStdString(
            boost::uuids::to_string(*conn.folder_id));
        int index = folderCombo_->findData(folderIdStr);
        if (index >= 0)
            folderCombo_->setCurrentIndex(index);
    } else {
        folderCombo_->setCurrentIndex(0);
    }

    setItemType(ItemType::Connection);
    setCreateMode(false);
}

connections::domain::connection AddItemDialog::getConnection() const {
    connections::domain::connection conn;

    if (isCreateMode_) {
        conn.id = boost::uuids::random_generator()();
    } else {
        conn.id = connectionId_;
    }

    conn.name = nameEdit_->text().trimmed().toStdString();
    conn.username = usernameEdit_->text().trimmed().toStdString();
    conn.description = descriptionEdit_->toPlainText().trimmed().toStdString();

    if (linkedEnvironmentId_) {
        conn.environment_id = linkedEnvironmentId_;
        // host/port left as nullopt — resolved from environment at connect time
    } else {
        conn.host = hostEdit_->text().trimmed().toStdString();
        conn.port = portSpinBox_->value();
    }

    QString folderIdStr = folderCombo_->currentData().toString();
    if (!folderIdStr.isEmpty()) {
        boost::uuids::string_generator gen;
        conn.folder_id = gen(folderIdStr.toStdString());
    }

    return conn;
}

void AddItemDialog::setInitialFolder(const std::optional<boost::uuids::uuid>& folderId) {
    if (folderId) {
        QString folderIdStr = QString::fromStdString(
            boost::uuids::to_string(*folderId));
        int index = folderCombo_->findData(folderIdStr);
        if (index >= 0)
            folderCombo_->setCurrentIndex(index);
    }
}

std::optional<std::string> AddItemDialog::getPassword() const {
    if (isCreateMode_) {
        return passwordEdit_->text().toStdString();
    }

    if (passwordChanged_) {
        return passwordEdit_->text().toStdString();
    }

    return std::nullopt;
}

void AddItemDialog::setTags(const std::vector<connections::domain::tag>& tags) {
    tagSelector_->setSelectedTags(tags);
}

std::vector<boost::uuids::uuid> AddItemDialog::getSelectedTagIds() const {
    return tagSelector_->selectedTagIds();
}

void AddItemDialog::setTestCallback(TestConnectionCallback callback) {
    testCallback_ = std::move(callback);
    testAction_->setVisible(itemType_ == ItemType::Connection && static_cast<bool>(testCallback_));
}

void AddItemDialog::onPasswordChanged() {
    passwordChanged_ = true;
}

void AddItemDialog::togglePasswordVisibility() {
    passwordEdit_->setEchoMode(
        showPasswordCheckbox_->isChecked() ? QLineEdit::Normal : QLineEdit::Password);
}

QString AddItemDialog::itemName() const {
    return nameEdit_->text().trimmed();
}

bool AddItemDialog::validateInput() {
    QString name = nameEdit_->text().trimmed();

    if (name.isEmpty()) {
        QMessageBox::warning(this, tr("Validation Error"),
            tr("Name cannot be empty."));
        nameEdit_->setFocus();
        return false;
    }

    if (itemType_ == ItemType::Environment) {
        QString host = hostEdit_->text().trimmed();
        if (host.isEmpty()) {
            QMessageBox::warning(this, tr("Validation Error"),
                tr("Host cannot be empty for an environment."));
            hostEdit_->setFocus();
            return false;
        }
    } else if (itemType_ == ItemType::Connection) {
        // Must have either a linked environment or a manual host
        if (!linkedEnvironmentId_ && hostEdit_->text().trimmed().isEmpty()) {
            QMessageBox::warning(this, tr("Validation Error"),
                tr("Please select an environment or enter a host."));
            hostEdit_->setFocus();
            return false;
        }

        if (usernameEdit_->text().trimmed().isEmpty()) {
            QMessageBox::warning(this, tr("Validation Error"),
                tr("Username cannot be empty."));
            usernameEdit_->setFocus();
            return false;
        }

        // Validate password if provided
        QString password = passwordEdit_->text();
        if (!password.isEmpty()) {
            if (isCreateMode_ || passwordChanged_) {
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
    }

    return true;
}

void AddItemDialog::onSaveClicked() {
    if (!validateInput()) {
        return;
    }

    if (itemType_ == ItemType::Folder) {
        saveFolder();
    } else if (itemType_ == ItemType::Environment) {
        saveEnvironment();
    } else {
        saveConnection();
    }
}

void AddItemDialog::saveFolder() {
    using namespace ores::logging;

    try {
        auto folder = getFolder();
        if (isCreateMode_) {
            manager_->create_folder(folder);
            BOOST_LOG_SEV(lg(), info) << "Created folder: " << folder.name;
        } else {
            manager_->update_folder(folder);
            BOOST_LOG_SEV(lg(), info) << "Updated folder: " << folder.name;
        }

        emit statusMessage(tr("Folder saved: %1").arg(QString::fromStdString(folder.name)));
        emit folderSaved(folder.id, QString::fromStdString(folder.name));

        if (auto* parent = parentWidget()) {
            parent->close();
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save folder: " << e.what();
        emit errorMessage(tr("Failed to save folder: %1").arg(e.what()));
    }
}

void AddItemDialog::saveEnvironment() {
    using namespace ores::logging;

    try {
        auto env = getEnvironment();
        if (isCreateMode_) {
            manager_->create_environment(env);
            BOOST_LOG_SEV(lg(), info) << "Created environment: " << env.name;
        } else {
            manager_->update_environment(env);
            BOOST_LOG_SEV(lg(), info) << "Updated environment: " << env.name;
        }

        // Handle tags
        auto tagIds = getSelectedTagIds();
        if (!isCreateMode_) {
            auto oldTags = manager_->get_tags_for_environment(env.id);
            for (const auto& oldTag : oldTags) {
                manager_->remove_tag_from_environment(env.id, oldTag.id);
            }
        }
        for (const auto& tagId : tagIds) {
            manager_->add_tag_to_environment(env.id, tagId);
        }

        emit statusMessage(tr("Environment saved: %1").arg(QString::fromStdString(env.name)));
        emit environmentSaved(env.id, QString::fromStdString(env.name));

        if (auto* parent = parentWidget()) {
            parent->close();
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save environment: " << e.what();
        emit errorMessage(tr("Failed to save environment: %1").arg(e.what()));
    }
}

void AddItemDialog::saveConnection() {
    using namespace ores::logging;

    try {
        auto conn = getConnection();
        auto password = getPassword();

        if (isCreateMode_) {
            manager_->create_connection(conn, password.value_or(""));
            BOOST_LOG_SEV(lg(), info) << "Created connection: " << conn.name;
        } else {
            manager_->update_connection(conn, password);
            BOOST_LOG_SEV(lg(), info) << "Updated connection: " << conn.name;
        }

        // Handle tags
        auto tagIds = getSelectedTagIds();
        if (!isCreateMode_) {
            auto oldTags = manager_->get_tags_for_connection(conn.id);
            for (const auto& oldTag : oldTags) {
                manager_->remove_tag_from_connection(conn.id, oldTag.id);
            }
        }
        for (const auto& tagId : tagIds) {
            manager_->add_tag_to_connection(conn.id, tagId);
        }

        emit statusMessage(tr("Connection saved: %1").arg(QString::fromStdString(conn.name)));
        emit connectionSaved(conn.id, QString::fromStdString(conn.name));

        if (auto* parent = parentWidget()) {
            parent->close();
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to save connection: " << e.what();
        emit errorMessage(tr("Failed to save connection: %1").arg(e.what()));
    }
}

void AddItemDialog::onTestClicked() {
    using namespace ores::logging;

    if (!testCallback_) {
        return;
    }

    QString host = hostEdit_->text().trimmed();
    QString username = usernameEdit_->text().trimmed();
    QString password = passwordEdit_->text();
    int port = portSpinBox_->value();

    if (host.isEmpty() || username.isEmpty()) {
        QMessageBox::warning(this, tr("Test Connection"),
            tr("Please enter host and username to test the connection."));
        return;
    }

    // If editing and password not changed, use saved password
    if (!isCreateMode_ && password.isEmpty() && !passwordChanged_) {
        try {
            password = QString::fromStdString(manager_->get_password(connectionId_));
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to retrieve saved password: " << e.what();
            QMessageBox::warning(this, tr("Test Connection"),
                tr("Please enter password to test the connection."));
            return;
        }
    }

    if (password.isEmpty()) {
        QMessageBox::warning(this, tr("Test Connection"),
            tr("Please enter password to test the connection."));
        return;
    }

    testAction_->setEnabled(false);
    testAction_->setText(tr("Testing..."));

    BOOST_LOG_SEV(lg(), info) << "Testing connection to " << host.toStdString()
                              << ":" << port;

    auto* watcher = new QFutureWatcher<QString>(this);
    connect(watcher, &QFutureWatcher<QString>::finished, this, [this, watcher]() {
        QString error = watcher->result();
        watcher->deleteLater();

        testAction_->setEnabled(true);
        testAction_->setText(tr("Test"));

        if (error.isEmpty()) {
            QMessageBox::information(this, tr("Test Connection"),
                tr("Connection successful!"));
            using namespace ores::logging;
            BOOST_LOG_SEV(lg(), info) << "Connection test successful";
        } else {
            QMessageBox::warning(this, tr("Test Connection"),
                tr("Connection failed: %1").arg(error));
            using namespace ores::logging;
            BOOST_LOG_SEV(lg(), warn) << "Connection test failed: " << error.toStdString();
        }
    });

    QFuture<QString> future = QtConcurrent::run(
        [cb = testCallback_, host, port, username, password]() {
            return cb(host, port, username, password);
        }
    );
    watcher->setFuture(future);
}

}
