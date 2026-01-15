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
#include "ores.qt/ConnectionDetailPanel.hpp"
#include "ores.connections/service/connection_manager.hpp"
#include <QHBoxLayout>
#include <QApplication>
#include <QMessageBox>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

namespace {

// Predefined tag colors - consistent with ConnectionItemDelegate
const std::vector<QColor> tag_colors = {
    QColor(59, 130, 246),   // Blue
    QColor(34, 197, 94),    // Green
    QColor(234, 179, 8),    // Amber
    QColor(239, 68, 68),    // Red
    QColor(168, 85, 247),   // Purple
    QColor(236, 72, 153),   // Pink
    QColor(20, 184, 166),   // Teal
    QColor(249, 115, 22),   // Orange
};

QColor colorForTag(const QString& name) {
    uint hash = qHash(name);
    return tag_colors[hash % tag_colors.size()];
}

QLabel* createTagBadge(const QString& text, QWidget* parent) {
    auto* badge = new QLabel(text, parent);
    QColor bgColor = colorForTag(text);
    badge->setStyleSheet(QString(
        "QLabel {"
        "  background-color: %1;"
        "  color: white;"
        "  border-radius: 8px;"
        "  padding: 2px 8px;"
        "  font-size: 11px;"
        "  font-weight: bold;"
        "}"
    ).arg(bgColor.name()));
    return badge;
}

}

ConnectionDetailPanel::ConnectionDetailPanel(
    connections::service::connection_manager* manager,
    QWidget* parent)
    : QWidget(parent),
      manager_(manager) {

    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);

    stackedWidget_ = new QStackedWidget(this);

    setupEmptyPage();
    setupFolderPage();
    setupEnvironmentPage();

    stackedWidget_->addWidget(emptyPage_);
    stackedWidget_->addWidget(folderPage_);
    stackedWidget_->addWidget(environmentPage_);

    mainLayout->addWidget(stackedWidget_);

    showEmptyState();
}

ConnectionDetailPanel::~ConnectionDetailPanel() = default;

void ConnectionDetailPanel::setTestCallback(TestConnectionCallback callback) {
    testCallback_ = std::move(callback);
    envTestButton_->setVisible(static_cast<bool>(testCallback_));
}

void ConnectionDetailPanel::setupEmptyPage() {
    emptyPage_ = new QWidget(this);
    auto* layout = new QVBoxLayout(emptyPage_);
    layout->setContentsMargins(24, 24, 24, 24);

    layout->addStretch();

    auto* titleLabel = new QLabel(tr("Connection Browser"), emptyPage_);
    titleLabel->setStyleSheet("font-size: 18px; font-weight: bold; color: #e0e0e0;");
    titleLabel->setAlignment(Qt::AlignCenter);
    layout->addWidget(titleLabel);

    auto* subtitleLabel = new QLabel(
        tr("Select a connection or folder to view details,\n"
           "or use the buttons below to get started."),
        emptyPage_);
    subtitleLabel->setStyleSheet("font-size: 13px; color: #909090;");
    subtitleLabel->setAlignment(Qt::AlignCenter);
    layout->addWidget(subtitleLabel);

    layout->addSpacing(24);

    // Quick action buttons
    auto* buttonLayout = new QHBoxLayout();
    buttonLayout->setSpacing(12);

    auto* addFolderBtn = new QPushButton(tr("New Folder"), emptyPage_);
    addFolderBtn->setMinimumHeight(36);
    connect(addFolderBtn, &QPushButton::clicked,
            this, &ConnectionDetailPanel::createFolderRequested);
    buttonLayout->addWidget(addFolderBtn);

    auto* addConnectionBtn = new QPushButton(tr("New Connection"), emptyPage_);
    addConnectionBtn->setMinimumHeight(36);
    connect(addConnectionBtn, &QPushButton::clicked,
            this, &ConnectionDetailPanel::createConnectionRequested);
    buttonLayout->addWidget(addConnectionBtn);

    layout->addLayout(buttonLayout);

    layout->addStretch();
}

void ConnectionDetailPanel::setupFolderPage() {
    folderPage_ = new QWidget(this);
    auto* layout = new QVBoxLayout(folderPage_);
    layout->setContentsMargins(24, 24, 24, 24);

    // Folder icon and name
    folderNameLabel_ = new QLabel(folderPage_);
    folderNameLabel_->setStyleSheet("font-size: 18px; font-weight: bold; color: #e0e0e0;");
    layout->addWidget(folderNameLabel_);

    layout->addSpacing(16);

    // Item count
    auto* countLabel = new QLabel(tr("Contents"), folderPage_);
    countLabel->setStyleSheet("font-size: 11px; color: #707070; text-transform: uppercase;");
    layout->addWidget(countLabel);

    folderItemCountLabel_ = new QLabel(folderPage_);
    folderItemCountLabel_->setStyleSheet("font-size: 13px; color: #c0c0c0;");
    layout->addWidget(folderItemCountLabel_);

    layout->addStretch();
}

void ConnectionDetailPanel::setupEnvironmentPage() {
    environmentPage_ = new QWidget(this);
    auto* layout = new QVBoxLayout(environmentPage_);
    layout->setContentsMargins(24, 24, 24, 24);

    // Connection name
    envNameLabel_ = new QLabel(environmentPage_);
    envNameLabel_->setStyleSheet("font-size: 18px; font-weight: bold; color: #e0e0e0;");
    layout->addWidget(envNameLabel_);

    layout->addSpacing(16);

    // Tags container
    envTagsContainer_ = new QWidget(environmentPage_);
    envTagsContainer_->setLayout(new QHBoxLayout());
    envTagsContainer_->layout()->setContentsMargins(0, 0, 0, 0);
    envTagsContainer_->layout()->setSpacing(6);
    layout->addWidget(envTagsContainer_);

    layout->addSpacing(16);

    // Connection details form
    auto* formLayout = new QFormLayout();
    formLayout->setLabelAlignment(Qt::AlignRight);
    formLayout->setSpacing(8);

    auto labelStyle = QString("font-size: 11px; color: #707070; text-transform: uppercase;");
    auto valueStyle = QString("font-size: 13px; color: #c0c0c0;");

    auto* hostLabel = new QLabel(tr("Host"), environmentPage_);
    hostLabel->setStyleSheet(labelStyle);
    envHostLabel_ = new QLabel(environmentPage_);
    envHostLabel_->setStyleSheet(valueStyle);
    formLayout->addRow(hostLabel, envHostLabel_);

    auto* portLabel = new QLabel(tr("Port"), environmentPage_);
    portLabel->setStyleSheet(labelStyle);
    envPortLabel_ = new QLabel(environmentPage_);
    envPortLabel_->setStyleSheet(valueStyle);
    formLayout->addRow(portLabel, envPortLabel_);

    auto* usernameLabel = new QLabel(tr("Username"), environmentPage_);
    usernameLabel->setStyleSheet(labelStyle);
    envUsernameLabel_ = new QLabel(environmentPage_);
    envUsernameLabel_->setStyleSheet(valueStyle);
    formLayout->addRow(usernameLabel, envUsernameLabel_);

    layout->addLayout(formLayout);

    layout->addSpacing(16);

    // Description
    auto* descHeaderLabel = new QLabel(tr("Description"), environmentPage_);
    descHeaderLabel->setStyleSheet(labelStyle);
    layout->addWidget(descHeaderLabel);

    envDescriptionLabel_ = new QLabel(environmentPage_);
    envDescriptionLabel_->setStyleSheet(valueStyle);
    envDescriptionLabel_->setWordWrap(true);
    layout->addWidget(envDescriptionLabel_);

    layout->addStretch();

    // Password and connect section
    auto* connectSection = new QWidget(environmentPage_);
    auto* connectLayout = new QVBoxLayout(connectSection);
    connectLayout->setContentsMargins(0, 0, 0, 0);
    connectLayout->setSpacing(12);

    auto* passwordLabel = new QLabel(tr("Password"), environmentPage_);
    passwordLabel->setStyleSheet(labelStyle);
    connectLayout->addWidget(passwordLabel);

    envPasswordEdit_ = new QLineEdit(environmentPage_);
    envPasswordEdit_->setEchoMode(QLineEdit::Password);
    envPasswordEdit_->setPlaceholderText(tr("Enter password to connect"));
    connectLayout->addWidget(envPasswordEdit_);

    // Button row
    auto* buttonLayout = new QHBoxLayout();
    buttonLayout->setSpacing(8);

    envEditButton_ = new QPushButton(tr("Edit"), environmentPage_);
    envEditButton_->setMinimumHeight(36);
    connect(envEditButton_, &QPushButton::clicked,
            this, &ConnectionDetailPanel::editRequested);
    buttonLayout->addWidget(envEditButton_);

    envTestButton_ = new QPushButton(tr("Test"), environmentPage_);
    envTestButton_->setMinimumHeight(36);
    envTestButton_->setVisible(false);
    connect(envTestButton_, &QPushButton::clicked,
            this, &ConnectionDetailPanel::onTestClicked);
    buttonLayout->addWidget(envTestButton_);

    envConnectButton_ = new QPushButton(tr("Connect"), environmentPage_);
    envConnectButton_->setMinimumHeight(36);
    envConnectButton_->setStyleSheet(
        "QPushButton { background-color: #3b82f6; color: white; font-weight: bold; }"
        "QPushButton:hover { background-color: #2563eb; }"
        "QPushButton:pressed { background-color: #1d4ed8; }"
    );
    connect(envConnectButton_, &QPushButton::clicked,
            this, &ConnectionDetailPanel::onConnectClicked);
    buttonLayout->addWidget(envConnectButton_);

    connectLayout->addLayout(buttonLayout);
    layout->addWidget(connectSection);
}

void ConnectionDetailPanel::showEmptyState() {
    currentEnvironment_.reset();
    stackedWidget_->setCurrentWidget(emptyPage_);
}

void ConnectionDetailPanel::showFolder(const connections::domain::folder& folder, int itemCount) {
    currentEnvironment_.reset();

    folderNameLabel_->setText(QString::fromStdString(folder.name));

    if (itemCount == 0) {
        folderItemCountLabel_->setText(tr("Empty folder"));
    } else if (itemCount == 1) {
        folderItemCountLabel_->setText(tr("1 item"));
    } else {
        folderItemCountLabel_->setText(tr("%1 items").arg(itemCount));
    }

    stackedWidget_->setCurrentWidget(folderPage_);
}

void ConnectionDetailPanel::showEnvironment(const connections::domain::server_environment& env) {
    currentEnvironment_ = env;

    envNameLabel_->setText(QString::fromStdString(env.name));
    envHostLabel_->setText(QString::fromStdString(env.host));
    envPortLabel_->setText(QString::number(env.port));
    envUsernameLabel_->setText(QString::fromStdString(env.username));

    if (env.description.empty()) {
        envDescriptionLabel_->setText(tr("No description"));
        envDescriptionLabel_->setStyleSheet("font-size: 13px; color: #606060; font-style: italic;");
    } else {
        envDescriptionLabel_->setText(QString::fromStdString(env.description));
        envDescriptionLabel_->setStyleSheet("font-size: 13px; color: #c0c0c0;");
    }

    // Clear password field for new selection
    envPasswordEdit_->clear();

    // Update tags
    updateTagBadges(env.id);

    stackedWidget_->setCurrentWidget(environmentPage_);
}

void ConnectionDetailPanel::updateTagBadges(const boost::uuids::uuid& envId) {
    // Clear existing badges
    QLayoutItem* item;
    while ((item = envTagsContainer_->layout()->takeAt(0)) != nullptr) {
        delete item->widget();
        delete item;
    }

    try {
        auto tags = manager_->get_tags_for_environment(envId);
        for (const auto& tag : tags) {
            auto* badge = createTagBadge(QString::fromStdString(tag.name), envTagsContainer_);
            envTagsContainer_->layout()->addWidget(badge);
        }

        // Add stretch at end
        static_cast<QHBoxLayout*>(envTagsContainer_->layout())->addStretch();

        envTagsContainer_->setVisible(!tags.empty());
    } catch (...) {
        envTagsContainer_->setVisible(false);
    }
}

void ConnectionDetailPanel::onConnectClicked() {
    if (!currentEnvironment_) {
        return;
    }

    QString password = envPasswordEdit_->text();
    if (password.isEmpty()) {
        QMessageBox::warning(this, tr("Connect"),
            tr("Please enter the password to connect."));
        envPasswordEdit_->setFocus();
        return;
    }

    emit connectRequested(currentEnvironment_->id, password);
}

void ConnectionDetailPanel::onTestClicked() {
    using namespace ores::logging;

    if (!currentEnvironment_ || !testCallback_) {
        return;
    }

    QString host = QString::fromStdString(currentEnvironment_->host);
    QString username = QString::fromStdString(currentEnvironment_->username);
    QString password = envPasswordEdit_->text();
    int port = currentEnvironment_->port;

    if (password.isEmpty()) {
        QMessageBox::warning(this, tr("Test Connection"),
            tr("Please enter the password to test the connection."));
        envPasswordEdit_->setFocus();
        return;
    }

    envTestButton_->setEnabled(false);
    envTestButton_->setText(tr("Testing..."));
    QApplication::processEvents();

    BOOST_LOG_SEV(lg(), info) << "Testing connection to " << host.toStdString()
                              << ":" << port;

    QString error = testCallback_(host, port, username, password);

    envTestButton_->setEnabled(true);
    envTestButton_->setText(tr("Test"));

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
