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
#include "ores.qt/ChangePasswordDialog.hpp"
#include "ores.qt/DialogStyles.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/PartyPickerDialog.hpp"
#include "ores.utility/version/version.hpp"
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QSet>
#include <QStandardItemModel>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <algorithm>
#include <boost/uuid/uuid_io.hpp>

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

// UserRole: item type as int (0 = Environment, 1 = Connection)
// UserRole+1: item name as QString
constexpr int ItemTypeRole = Qt::UserRole;
constexpr int ItemNameRole = Qt::UserRole + 1;
constexpr int TypeEnvironment = static_cast<int>(LoginDialog::QuickConnectItem::Type::Environment);
constexpr int TypeConnection = static_cast<int>(LoginDialog::QuickConnectItem::Type::Connection);

}

LoginDialog::LoginDialog(QWidget* parent)
    : QWidget(parent) {
    setupUI();

    // Register LoginResult for cross-thread signal/slot
    qRegisterMetaType<LoginResult>("LoginResult");
}

LoginDialog::~LoginDialog() = default;

QSize LoginDialog::sizeHint() const {
    const bool filterVisible = labelFilterCombo_ && labelFilterCombo_->isVisible();
    return {400, filterVisible ? 720 : 580};
}

void LoginDialog::keyPressEvent(QKeyEvent* event) {
    if (event->key() == Qt::Key_Escape) {
        emit closeRequested();
    } else {
        QWidget::keyPressEvent(event);
    }
}

void LoginDialog::setImageCache(ImageCache* cache) {
    imageCache_ = cache;
}

void LoginDialog::setQuickConnectItems(const QList<QuickConnectItem>& items) {
    allItems_ = items;

    if (items.isEmpty()) {
        labelFilterLabel_->setVisible(false);
        labelFilterCombo_->setVisible(false);
        quickConnectLabel_->setVisible(false);
        quickConnectCombo_->setVisible(false);
        return;
    }

    // Collect all unique tags across all items
    QSet<QString> tagSet;
    for (const auto& it : items) {
        for (const auto& tag : it.tags)
            tagSet.insert(tag);
    }
    QStringList sortedTags(tagSet.begin(), tagSet.end());
    sortedTags.sort(Qt::CaseInsensitive);

    // Show/hide label filter based on whether there are ≥2 distinct tags
    const bool showFilter = sortedTags.size() >= 2;
    labelFilterLabel_->setVisible(showFilter);
    labelFilterCombo_->setVisible(showFilter);
    updateGeometry();

    if (showFilter) {
        // Rebuild filter combo without triggering applyLabelFilter during population
        QSignalBlocker blocker(labelFilterCombo_);
        labelFilterCombo_->clear();
        labelFilterCombo_->addItem(tr("All"));
        for (const auto& tag : sortedTags)
            labelFilterCombo_->addItem(tag);
        labelFilterCombo_->setCurrentIndex(0);
    }

    // Show all items initially (or filtered if setActiveLabel was already called)
    applyLabelFilter(showFilter ? labelFilterCombo_->currentText() : QString());
}

void LoginDialog::setActiveLabel(const QString& label) {
    if (!labelFilterCombo_ || !labelFilterCombo_->isVisible() || label.isEmpty())
        return;
    const int idx = labelFilterCombo_->findText(label, Qt::MatchFixedString);
    if (idx >= 0)
        labelFilterCombo_->setCurrentIndex(idx);
    // If not found, leave at "All" — applyLabelFilter will show everything
}

void LoginDialog::applyLabelFilter(const QString& label) {
    if (label.isEmpty() || label == tr("All")) {
        rebuildQuickConnectModel(allItems_);
    } else {
        QList<QuickConnectItem> filtered;
        for (const auto& it : allItems_) {
            if (it.tags.contains(label, Qt::CaseSensitive))
                filtered.append(it);
        }
        rebuildQuickConnectModel(filtered);
    }
}

void LoginDialog::rebuildQuickConnectModel(const QList<QuickConnectItem>& items) {
    auto* model = new QStandardItemModel(quickConnectCombo_);

    auto makeHeader = [](const QString& text) -> QStandardItem* {
        auto* h = new QStandardItem(text);
        h->setFlags(Qt::NoItemFlags);
        h->setData(QColor("#666666"), Qt::ForegroundRole);
        QFont f;
        f.setPointSize(9);
        f.setItalic(true);
        h->setData(f, Qt::FontRole);
        return h;
    };

    // Manual section (always first) — keyboard icon, type = -1
    model->appendRow(makeHeader(tr("  Manual")));
    auto manualIcon = IconUtils::createRecoloredIcon(Icon::Keyboard, QColor("#808080"));
    auto* manualItem = new QStandardItem(manualIcon, tr("  Enter details manually"));
    manualItem->setData(static_cast<int>(-1), ItemTypeRole);
    model->appendRow(manualItem);

    // Collect by type
    QList<const QuickConnectItem*> envs, conns;
    for (const auto& it : items) {
        if (it.type == QuickConnectItem::Type::Environment)
            envs.append(&it);
        else
            conns.append(&it);
    }

    if (!envs.isEmpty()) {
        model->appendRow(makeHeader(tr("  Environments")));
        auto icon = IconUtils::createRecoloredIcon(Icon::Server, QColor("#9090b0"));
        for (const auto* it : envs) {
            const QString text =
                it->subtitle.isEmpty() ? it->name : it->name + "   " + it->subtitle;
            auto* row = new QStandardItem(icon, text);
            row->setData(TypeEnvironment, ItemTypeRole);
            row->setData(it->name, ItemNameRole);
            model->appendRow(row);
        }
    }

    if (!conns.isEmpty()) {
        model->appendRow(makeHeader(tr("  Connections")));
        auto icon = IconUtils::createRecoloredIcon(Icon::PlugConnected, QColor("#90b090"));
        for (const auto* it : conns) {
            const QString text =
                it->subtitle.isEmpty() ? it->name : it->name + "   [" + it->subtitle + "]";
            auto* row = new QStandardItem(icon, text);
            row->setData(TypeConnection, ItemTypeRole);
            row->setData(it->name, ItemNameRole);
            model->appendRow(row);
        }
    }

    quickConnectCombo_->setModel(model);
    quickConnectCombo_->setCurrentIndex(1); // default to "Enter details manually"
    quickConnectLabel_->setVisible(true);
    quickConnectCombo_->setVisible(true);
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

void LoginDialog::setSubjectPrefix(const QString& prefix) {
    subjectPrefixEdit_->setText(prefix);
}

void LoginDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void LoginDialog::setConnectionManager(
    ores::connections::service::connection_manager* connectionManager) {
    connectionManager_ = connectionManager;
}

QString LoginDialog::getUsername() const {
    return usernameEdit_->text().trimmed();
}

QString LoginDialog::getPassword() const {
    return passwordEdit_->text();
}

QString LoginDialog::getServer() const {
    return hostEdit_->text().trimmed();
}

int LoginDialog::getPort() const {
    return portSpinBox_->value();
}

QString LoginDialog::getSubjectPrefix() const {
    return subjectPrefixEdit_->text().trimmed();
}

void LoginDialog::lockServerFields(bool locked) {
    serverFieldsLocked_ = locked;
    hostEdit_->setReadOnly(locked);
    portSpinBox_->setReadOnly(locked);
    subjectPrefixEdit_->setReadOnly(locked);
    hostEdit_->setStyleSheet(locked ? dialog_styles::input_field_locked :
                                      dialog_styles::input_field);
    portSpinBox_->setStyleSheet(locked ? dialog_styles::spin_box_locked : dialog_styles::spin_box);
    subjectPrefixEdit_->setStyleSheet(locked ? dialog_styles::input_field_locked :
                                               dialog_styles::input_field);
}

void LoginDialog::lockCredentialFields(bool locked) {
    credentialFieldsLocked_ = locked;
    usernameEdit_->setReadOnly(locked);
    passwordEdit_->setReadOnly(locked);
    usernameEdit_->setStyleSheet(locked ? dialog_styles::input_field_locked :
                                          dialog_styles::input_field);
    passwordEdit_->setStyleSheet(locked ? dialog_styles::input_field_locked :
                                          dialog_styles::input_field);
}

void LoginDialog::enableForm(bool enabled) {
    loginButton_->setEnabled(enabled);
    signUpButton_->setEnabled(enabled);
    if (labelFilterCombo_)
        labelFilterCombo_->setEnabled(enabled);
    quickConnectCombo_->setEnabled(enabled);

    if (enabled) {
        // Restore the lock state that was active before the login attempt
        hostEdit_->setEnabled(true);
        portSpinBox_->setEnabled(true);
        subjectPrefixEdit_->setEnabled(true);
        usernameEdit_->setEnabled(true);
        passwordEdit_->setEnabled(true);
        lockServerFields(serverFieldsLocked_);
        lockCredentialFields(credentialFieldsLocked_);
    } else {
        // Disable everything while waiting for the server response
        hostEdit_->setEnabled(false);
        portSpinBox_->setEnabled(false);
        subjectPrefixEdit_->setEnabled(false);
        usernameEdit_->setEnabled(false);
        passwordEdit_->setEnabled(false);
    }
}

void LoginDialog::setupUI() {
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);
    mainLayout->setSpacing(0);

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
    auto* usernameLabel = new QLabel("USERNAME", parent);
    usernameLabel->setStyleSheet(dialog_styles::field_label);
    layout->addWidget(usernameLabel);
    layout->addSpacing(6);

    usernameEdit_ = new QLineEdit(parent);
    usernameEdit_->setPlaceholderText("Enter your username");
    usernameEdit_->setStyleSheet(dialog_styles::input_field);
    usernameEdit_->setFixedHeight(36);
    layout->addWidget(usernameEdit_);

    layout->addSpacing(12);

    auto* passwordLabel = new QLabel("PASSWORD", parent);
    passwordLabel->setStyleSheet(dialog_styles::field_label);
    layout->addWidget(passwordLabel);
    layout->addSpacing(6);

    passwordEdit_ = new QLineEdit(parent);
    passwordEdit_->setPlaceholderText("Enter your password");
    passwordEdit_->setEchoMode(QLineEdit::Password);
    passwordEdit_->setStyleSheet(dialog_styles::input_field);
    passwordEdit_->setFixedHeight(36);
    layout->addWidget(passwordEdit_);

    layout->addSpacing(12);

    auto* optionsRow = new QHBoxLayout();
    showPasswordCheck_ = new QCheckBox("Show password", parent);
    showPasswordCheck_->setStyleSheet(dialog_styles::checkbox);
    connect(showPasswordCheck_, &QCheckBox::toggled, this, &LoginDialog::onShowPasswordToggled);

    rememberMeCheck_ = new QCheckBox("Remember me", parent);
    rememberMeCheck_->setStyleSheet(dialog_styles::checkbox);

    quickLoginCheck_ = new QCheckBox("Log in to default party", parent);
    quickLoginCheck_->setStyleSheet(dialog_styles::checkbox);
    quickLoginCheck_->setChecked(true);

    optionsRow->addWidget(showPasswordCheck_);
    optionsRow->addStretch();
    optionsRow->addWidget(rememberMeCheck_);
    layout->addLayout(optionsRow);

    auto* quickLoginRow = new QHBoxLayout();
    quickLoginRow->addWidget(quickLoginCheck_);
    quickLoginRow->addStretch();
    layout->addLayout(quickLoginRow);

    layout->addSpacing(12);
}

void LoginDialog::setupServerFields(QVBoxLayout* layout, QWidget* parent) {
    // Label filter — hidden until setQuickConnectItems() finds ≥2 distinct tags.
    labelFilterLabel_ = new QLabel(tr("LABEL"), parent);
    labelFilterLabel_->setStyleSheet(dialog_styles::field_label);
    labelFilterLabel_->setVisible(false);
    layout->addWidget(labelFilterLabel_);
    layout->addSpacing(6);

    labelFilterCombo_ = new QComboBox(parent);
    labelFilterCombo_->setStyleSheet(dialog_styles::combo_box);
    labelFilterCombo_->setFixedHeight(36);
    labelFilterCombo_->setVisible(false);
    layout->addWidget(labelFilterCombo_);

    connect(labelFilterCombo_,
            &QComboBox::currentIndexChanged,
            this,
            &LoginDialog::onLabelFilterChanged);

    layout->addSpacing(12);

    // Quick-connect combo: environments (fills host+port) and connections
    // (fills all fields). Hidden until setQuickConnectItems() is called.
    quickConnectLabel_ = new QLabel(tr("QUICK CONNECT"), parent);
    quickConnectLabel_->setStyleSheet(dialog_styles::field_label);
    quickConnectLabel_->setVisible(false);
    layout->addWidget(quickConnectLabel_);
    layout->addSpacing(6);

    quickConnectCombo_ = new QComboBox(parent);
    quickConnectCombo_->addItem(tr("— connect manually —"));
    quickConnectCombo_->setStyleSheet(dialog_styles::combo_box);
    quickConnectCombo_->setFixedHeight(36);
    quickConnectCombo_->setVisible(false);
    layout->addWidget(quickConnectCombo_);

    connect(quickConnectCombo_,
            &QComboBox::currentIndexChanged,
            this,
            &LoginDialog::onQuickConnectChanged);

    layout->addSpacing(12);

    auto* hostLabel = new QLabel("SERVER", parent);
    hostLabel->setStyleSheet(dialog_styles::field_label);
    layout->addWidget(hostLabel);
    layout->addSpacing(6);

    hostEdit_ = new QLineEdit(parent);
    hostEdit_->setPlaceholderText("localhost");
    hostEdit_->setText("localhost");
    hostEdit_->setStyleSheet(dialog_styles::input_field);
    hostEdit_->setFixedHeight(36);
    layout->addWidget(hostEdit_);

    layout->addSpacing(12);

    auto* portLabel = new QLabel("PORT", parent);
    portLabel->setStyleSheet(dialog_styles::field_label);
    layout->addWidget(portLabel);
    layout->addSpacing(6);

    portSpinBox_ = new QSpinBox(parent);
    portSpinBox_->setRange(1, 65535);
    portSpinBox_->setValue(4222);
    portSpinBox_->setStyleSheet(dialog_styles::spin_box);
    portSpinBox_->setFixedHeight(36);
    layout->addWidget(portSpinBox_);

    layout->addSpacing(12);

    auto* prefixLabel = new QLabel("NAMESPACE", parent);
    prefixLabel->setStyleSheet(dialog_styles::field_label);
    layout->addWidget(prefixLabel);
    layout->addSpacing(6);

    subjectPrefixEdit_ = new QLineEdit(parent);
    subjectPrefixEdit_->setPlaceholderText("e.g. ores.dev.local1  (leave empty for default)");
    subjectPrefixEdit_->setText("ores.dev.local1");
    subjectPrefixEdit_->setStyleSheet(dialog_styles::input_field);
    subjectPrefixEdit_->setFixedHeight(36);
    layout->addWidget(subjectPrefixEdit_);

    layout->addSpacing(12);
}

void LoginDialog::setupActions(QVBoxLayout* layout, QWidget* parent) {
    statusLabel_ = new QLabel(parent);
    statusLabel_->setStyleSheet(dialog_styles::status);
    statusLabel_->setAlignment(Qt::AlignCenter);
    layout->addWidget(statusLabel_);

    layout->addSpacing(4);

    loginButton_ = new QPushButton("Login", parent);
    loginButton_->setStyleSheet(dialog_styles::primary_button);
    loginButton_->setFixedHeight(40);
    loginButton_->setCursor(Qt::PointingHandCursor);
    connect(loginButton_, &QPushButton::clicked, this, &LoginDialog::onLoginClicked);
    layout->addWidget(loginButton_);

    layout->addSpacing(16);

    auto* signUpRow = new QHBoxLayout();
    signUpRow->setAlignment(Qt::AlignCenter);
    signUpRow->setSpacing(4);
    signUpLabel_ = new QLabel("Don't have an account?", parent);
    signUpLabel_->setStyleSheet(
        "QLabel { background: transparent; color: #707070; font-size: 12px; }");

    signUpButton_ = new QPushButton("Register", parent);
    signUpButton_->setStyleSheet(dialog_styles::link_button);
    signUpButton_->setCursor(Qt::PointingHandCursor);
    connect(signUpButton_, &QPushButton::clicked, this, &LoginDialog::onSignUpClicked);

    signUpRow->addWidget(signUpLabel_);
    signUpRow->addWidget(signUpButton_);
    layout->addLayout(signUpRow);
}

void LoginDialog::setupFooter(QVBoxLayout* layout, QWidget* parent) {
    QString versionText = QString("v%1  %2")
                              .arg(ORES_VERSION)
                              .arg(QString::fromStdString(ores::utility::version::build_info()));
    auto* versionLabel = new QLabel(versionText, parent);
    versionLabel->setStyleSheet(dialog_styles::version);
    layout->addWidget(versionLabel, 0, Qt::AlignCenter);

    layout->addSpacing(4);

    auto* copyrightLabel = new QLabel(QString::fromUtf8("\u00A9 2025 ORE Studio"), parent);
    copyrightLabel->setStyleSheet(dialog_styles::version);
    layout->addWidget(copyrightLabel, 0, Qt::AlignCenter);

    layout->addSpacing(8);
}

void LoginDialog::onLabelFilterChanged(int) {
    applyLabelFilter(labelFilterCombo_->currentText());
}

void LoginDialog::onQuickConnectChanged(int idx) {
    auto* model = qobject_cast<QStandardItemModel*>(quickConnectCombo_->model());
    if (!model)
        return;

    auto* item = model->item(idx);
    if (!item || !item->isEnabled())
        return; // section header — skip

    const int type = item->data(ItemTypeRole).toInt();

    if (type < 0) {
        // Manual entry — unlock all fields
        lockServerFields(false);
        lockCredentialFields(false);
        return;
    }

    const QString name = item->data(ItemNameRole).toString();

    if (type == TypeEnvironment) {
        // Environment: lock server fields, leave credentials editable
        lockServerFields(true);
        lockCredentialFields(false);
        usernameEdit_->clear();
        passwordEdit_->clear();
        emit environmentSelected(name);
    } else {
        // Connection: lock all fields
        lockServerFields(true);
        lockCredentialFields(true);
        emit connectionSelected(name);
    }
}

void LoginDialog::onLoginClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Login attempt initiated";

    if (!clientManager_) {
        BOOST_LOG_SEV(lg(), error) << "clientManager_ is null — cannot attempt login";
        MessageBoxHelper::critical(this, "Internal Error", "Client manager not initialized");
        return;
    }

    const auto username = usernameEdit_->text().trimmed();
    const auto password = passwordEdit_->text();
    const auto host = hostEdit_->text().trimmed();
    const auto port = static_cast<std::uint16_t>(portSpinBox_->value());
    const auto prefix = subjectPrefixEdit_->text().trimmed();
    clientManager_->setSubjectPrefix(prefix.toStdString());

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

    BOOST_LOG_SEV(lg(), debug) << "Connecting to " << host.toStdString() << ":" << port << " as '"
                               << username.toStdString() << "'" << " (namespace: '"
                               << prefix.toStdString() << "')";

    enableForm(false);
    statusLabel_->setText("Connecting to server...");
    statusLabel_->setStyleSheet(dialog_styles::status);

    auto* watcher = new QFutureWatcher<LoginResult>(this);
    connect(watcher, &QFutureWatcher<LoginResult>::finished, [this, watcher]() {
        try {
            const auto result = watcher->result();
            watcher->deleteLater();
            onLoginResult(result);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Login future threw unexpected exception: " << e.what();
            watcher->deleteLater();
            LoginResult r;
            r.error_message = QString::fromStdString(e.what());
            onLoginResult(r);
        } catch (...) {
            BOOST_LOG_SEV(lg(), error) << "Login future threw unknown exception";
            watcher->deleteLater();
            LoginResult r;
            r.error_message = "Unknown internal error during login";
            onLoginResult(r);
        }
    });

    QFuture<LoginResult> future =
        QtConcurrent::run([this, host, port, username, password]() -> LoginResult {
            BOOST_LOG_SEV(lg(), debug)
                << "Background login thread started for " << host.toStdString() << ":" << port;
            try {
                return clientManager_->connectAndLogin(
                    host.toStdString(), port, username.toStdString(), password.toStdString());
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error) << "connectAndLogin threw exception: " << e.what();
                LoginResult r;
                r.error_message = QString::fromStdString(e.what());
                return r;
            } catch (...) {
                BOOST_LOG_SEV(lg(), error) << "connectAndLogin threw unknown exception";
                LoginResult r;
                r.error_message = "Unknown internal error";
                return r;
            }
        });

    watcher->setFuture(future);
}

void LoginDialog::onLoginResult(const LoginResult& result) {
    BOOST_LOG_SEV(lg(), debug)
        << "Login result received: " << (result.success ? "success" : "failure")
        << (result.error_message.isEmpty() ? "" : " — " + result.error_message.toStdString());

    if (result.bootstrap_mode) {
        BOOST_LOG_SEV(lg(), info) << "System is in bootstrap mode - provisioning required";
        statusLabel_->setText("System requires provisioning...");
        emit bootstrapModeDetected();
        emit closeRequested();
        return;
    }

    if (result.success) {
        BOOST_LOG_SEV(lg(), debug) << "Login was successful";

        if (result.password_reset_required) {
            BOOST_LOG_SEV(lg(), info) << "Password reset required";
            statusLabel_->setText("Password change required...");

            ChangePasswordDialog changeDialog(clientManager_, this);
            if (changeDialog.exec() == QDialog::Accepted) {
                BOOST_LOG_SEV(lg(), info) << "Password changed successfully";
                statusLabel_->setText("Login successful!");
                emit loginSucceeded(QString::fromStdString(clientManager_->currentUsername()));
                emit closeRequested();
            } else {
                BOOST_LOG_SEV(lg(), warn) << "Password change canceled";
                clientManager_->disconnect();
                enableForm(true);
                statusLabel_->setText("");
                MessageBoxHelper::warning(
                    this,
                    "Password Change Required",
                    "You must change your password to continue. Please login again to retry.");
            }
        } else if (result.selected_party_id.is_nil() && !result.available_parties.empty()) {
            BOOST_LOG_SEV(lg(), info)
                << "Party selection required: " << result.available_parties.size()
                << " parties available";

            if (quickLoginCheck_->isChecked() && !result.default_party_id.is_nil()) {
                const auto it = std::find_if(
                    result.available_parties.begin(),
                    result.available_parties.end(),
                    [&](const auto& p) { return p.id == result.default_party_id; });
                if (it != result.available_parties.end()) {
                    BOOST_LOG_SEV(lg(), info)
                        << "Quick-login: selecting default party " << it->name.toStdString();
                    statusLabel_->setText("Logging in to default party...");
                    if (clientManager_->selectParty(it->id, it->name)) {
                        statusLabel_->setText("Login successful!");
                        emit loginSucceeded(QString::fromStdString(clientManager_->currentUsername()));
                        if (clientManager_->lastPartySetupRequired()) {
                            emit partySetupDetected();
                        } else if (!clientManager_->lastPartySetupWarning().isEmpty()) {
                            MessageBoxHelper::warning(
                                this, "Party Setup", clientManager_->lastPartySetupWarning());
                        }
                        emit closeRequested();
                        return;
                    }
                    BOOST_LOG_SEV(lg(), warn)
                        << "Quick-login: selectParty failed, falling back to party picker";
                }
            }

            statusLabel_->setText("Select party...");

            std::vector<boost::uuids::uuid> recentIds;
            if (connectionManager_) {
                try {
                    for (const auto& rp : connectionManager_->get_recent_parties())
                        recentIds.push_back(rp.party_id);
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(lg(), warn) << "Failed to load recent parties: " << e.what();
                }
            }

            PartyPickerDialog partyDialog(
                result.available_parties, recentIds, clientManager_, imageCache_, this);
            if (partyDialog.exec() == QDialog::Accepted) {
                BOOST_LOG_SEV(lg(), info)
                    << "Party selected: " << clientManager_->currentPartyName().toStdString()
                    << " (category=" << clientManager_->currentPartyCategory().toStdString() << ")";
                if (connectionManager_) {
                    try {
                        connectionManager_->record_party_selection(
                            partyDialog.selectedPartyId(),
                            partyDialog.selectedPartyName().toStdString());
                    } catch (const std::exception& e) {
                        BOOST_LOG_SEV(lg(), warn) << "Failed to record recent party: " << e.what();
                    }
                }
                statusLabel_->setText("Login successful!");
                emit loginSucceeded(QString::fromStdString(clientManager_->currentUsername()));
                if (clientManager_->lastPartySetupRequired()) {
                    BOOST_LOG_SEV(lg(), info)
                        << "Selected party is inactive - party provisioning wizard required"
                        << " party_id=" << clientManager_->currentPartyId()
                        << " party_name=" << clientManager_->currentPartyName().toStdString();
                    emit partySetupDetected();
                } else if (!clientManager_->lastPartySetupWarning().isEmpty()) {
                    BOOST_LOG_SEV(lg(), warn)
                        << "Party setup warning: "
                        << clientManager_->lastPartySetupWarning().toStdString();
                    MessageBoxHelper::warning(
                        this, "Party Setup", clientManager_->lastPartySetupWarning());
                }
                emit closeRequested();
            } else {
                BOOST_LOG_SEV(lg(), warn) << "Party selection canceled by user";
                clientManager_->disconnect();
                enableForm(true);
                statusLabel_->setText("");
                MessageBoxHelper::warning(
                    this, "Party Selection Required", "You must select a party to continue.");
            }
        } else if (result.tenant_bootstrap_mode) {
            BOOST_LOG_SEV(lg(), info)
                << "Tenant is in bootstrap mode - provisioning wizard required";
            statusLabel_->setText("Login successful!");
            emit loginSucceeded(QString::fromStdString(clientManager_->currentUsername()));
            emit tenantBootstrapDetected();
            emit closeRequested();
        } else if (result.party_setup_required) {
            BOOST_LOG_SEV(lg(), info)
                << "Party is inactive - party provisioning wizard required"
                << " party_id=" << clientManager_->currentPartyId()
                << " party_name=" << clientManager_->currentPartyName().toStdString();
            statusLabel_->setText("Login successful!");
            emit loginSucceeded(QString::fromStdString(clientManager_->currentUsername()));
            emit partySetupDetected();
            emit closeRequested();
        } else {
            if (!result.selected_party_id.is_nil()) {
                BOOST_LOG_SEV(lg(), info)
                    << "Party auto-selected: " << clientManager_->currentPartyName().toStdString()
                    << " (category=" << clientManager_->currentPartyCategory().toStdString()
                    << ", id=" << boost::uuids::to_string(result.selected_party_id) << ")";
            } else {
                BOOST_LOG_SEV(lg(), info) << "No party context for this account";
            }
            if (!result.party_setup_warning.isEmpty()) {
                BOOST_LOG_SEV(lg(), warn)
                    << "Party setup warning: " << result.party_setup_warning.toStdString();
                MessageBoxHelper::warning(this, "Party Setup", result.party_setup_warning);
            }
            statusLabel_->setText("Login successful!");
            emit loginSucceeded(QString::fromStdString(clientManager_->currentUsername()));
            emit closeRequested();
        }
    } else {
        BOOST_LOG_SEV(lg(), error) << "Login failed: " << result.error_message.toStdString();

        enableForm(true);
        statusLabel_->setText("Login failed.");

        emit loginFailed(result.error_message);
        MessageBoxHelper::critical(
            this, "Login Failed", QString("Authentication failed: %1").arg(result.error_message));
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
