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
#include "ores.qt/ConnectionTypes.hpp"
#include "ores.connections/service/connection_manager.hpp"
#include <QHBoxLayout>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

namespace {

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
    setupConnectionPage();

    stackedWidget_->addWidget(emptyPage_);
    stackedWidget_->addWidget(folderPage_);
    stackedWidget_->addWidget(environmentPage_);
    stackedWidget_->addWidget(connectionPage_);

    mainLayout->addWidget(stackedWidget_);

    showEmptyState();
}

ConnectionDetailPanel::~ConnectionDetailPanel() = default;

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
        tr("Select a connection or folder to view details."),
        emptyPage_);
    subtitleLabel->setStyleSheet("font-size: 13px; color: #909090;");
    subtitleLabel->setAlignment(Qt::AlignCenter);
    layout->addWidget(subtitleLabel);

    layout->addStretch();
}

void ConnectionDetailPanel::setupFolderPage() {
    folderPage_ = new QWidget(this);
    auto* layout = new QVBoxLayout(folderPage_);
    layout->setContentsMargins(24, 24, 24, 24);

    auto labelStyle = QString("font-size: 11px; color: #707070; text-transform: uppercase;");
    auto valueStyle = QString("font-size: 13px; color: #c0c0c0;");

    folderNameLabel_ = new QLabel(folderPage_);
    folderNameLabel_->setStyleSheet("font-size: 18px; font-weight: bold; color: #e0e0e0;");
    layout->addWidget(folderNameLabel_);

    layout->addSpacing(16);

    auto* countLabel = new QLabel(tr("Contents"), folderPage_);
    countLabel->setStyleSheet(labelStyle);
    layout->addWidget(countLabel);

    folderItemCountLabel_ = new QLabel(folderPage_);
    folderItemCountLabel_->setStyleSheet(valueStyle);
    layout->addWidget(folderItemCountLabel_);

    layout->addSpacing(16);

    auto* descHeaderLabel = new QLabel(tr("Description"), folderPage_);
    descHeaderLabel->setStyleSheet(labelStyle);
    layout->addWidget(descHeaderLabel);

    folderDescriptionLabel_ = new QLabel(folderPage_);
    folderDescriptionLabel_->setStyleSheet(valueStyle);
    folderDescriptionLabel_->setWordWrap(true);
    layout->addWidget(folderDescriptionLabel_);

    layout->addStretch();
}

void ConnectionDetailPanel::setupEnvironmentPage() {
    environmentPage_ = new QWidget(this);
    auto* layout = new QVBoxLayout(environmentPage_);
    layout->setContentsMargins(24, 24, 24, 24);

    auto labelStyle = QString("font-size: 11px; color: #707070; text-transform: uppercase;");
    auto valueStyle = QString("font-size: 13px; color: #c0c0c0;");

    envNameLabel_ = new QLabel(environmentPage_);
    envNameLabel_->setStyleSheet("font-size: 18px; font-weight: bold; color: #e0e0e0;");
    layout->addWidget(envNameLabel_);

    layout->addSpacing(16);

    envTagsContainer_ = new QWidget(environmentPage_);
    envTagsContainer_->setLayout(new QHBoxLayout());
    envTagsContainer_->layout()->setContentsMargins(0, 0, 0, 0);
    envTagsContainer_->layout()->setSpacing(6);
    layout->addWidget(envTagsContainer_);

    layout->addSpacing(16);

    auto* formLayout = new QFormLayout();
    formLayout->setLabelAlignment(Qt::AlignRight);
    formLayout->setSpacing(8);

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

    layout->addLayout(formLayout);

    layout->addSpacing(16);

    auto* descHeaderLabel = new QLabel(tr("Description"), environmentPage_);
    descHeaderLabel->setStyleSheet(labelStyle);
    layout->addWidget(descHeaderLabel);

    envDescriptionLabel_ = new QLabel(environmentPage_);
    envDescriptionLabel_->setStyleSheet(valueStyle);
    envDescriptionLabel_->setWordWrap(true);
    layout->addWidget(envDescriptionLabel_);

    layout->addStretch();
}

void ConnectionDetailPanel::setupConnectionPage() {
    connectionPage_ = new QWidget(this);
    auto* layout = new QVBoxLayout(connectionPage_);
    layout->setContentsMargins(24, 24, 24, 24);

    auto labelStyle = QString("font-size: 11px; color: #707070; text-transform: uppercase;");
    auto valueStyle = QString("font-size: 13px; color: #c0c0c0;");

    connNameLabel_ = new QLabel(connectionPage_);
    connNameLabel_->setStyleSheet("font-size: 18px; font-weight: bold; color: #e0e0e0;");
    layout->addWidget(connNameLabel_);

    layout->addSpacing(16);

    connTagsContainer_ = new QWidget(connectionPage_);
    connTagsContainer_->setLayout(new QHBoxLayout());
    connTagsContainer_->layout()->setContentsMargins(0, 0, 0, 0);
    connTagsContainer_->layout()->setSpacing(6);
    layout->addWidget(connTagsContainer_);

    layout->addSpacing(16);

    auto* formLayout = new QFormLayout();
    formLayout->setLabelAlignment(Qt::AlignRight);
    formLayout->setSpacing(8);

    auto* hostLabel = new QLabel(tr("Host"), connectionPage_);
    hostLabel->setStyleSheet(labelStyle);
    connHostLabel_ = new QLabel(connectionPage_);
    connHostLabel_->setStyleSheet(valueStyle);
    formLayout->addRow(hostLabel, connHostLabel_);

    auto* portLabel = new QLabel(tr("Port"), connectionPage_);
    portLabel->setStyleSheet(labelStyle);
    connPortLabel_ = new QLabel(connectionPage_);
    connPortLabel_->setStyleSheet(valueStyle);
    formLayout->addRow(portLabel, connPortLabel_);

    auto* usernameLabel = new QLabel(tr("Username"), connectionPage_);
    usernameLabel->setStyleSheet(labelStyle);
    connUsernameLabel_ = new QLabel(connectionPage_);
    connUsernameLabel_->setStyleSheet(valueStyle);
    formLayout->addRow(usernameLabel, connUsernameLabel_);

    layout->addLayout(formLayout);

    layout->addSpacing(16);

    auto* descHeaderLabel = new QLabel(tr("Description"), connectionPage_);
    descHeaderLabel->setStyleSheet(labelStyle);
    layout->addWidget(descHeaderLabel);

    connDescriptionLabel_ = new QLabel(connectionPage_);
    connDescriptionLabel_->setStyleSheet(valueStyle);
    connDescriptionLabel_->setWordWrap(true);
    layout->addWidget(connDescriptionLabel_);

    layout->addStretch();
}

void ConnectionDetailPanel::showEmptyState() {
    stackedWidget_->setCurrentWidget(emptyPage_);
}

void ConnectionDetailPanel::showFolder(const connections::domain::folder& folder, int itemCount) {
    folderNameLabel_->setText(QString::fromStdString(folder.name));

    if (itemCount == 0) {
        folderItemCountLabel_->setText(tr("Empty folder"));
    } else if (itemCount == 1) {
        folderItemCountLabel_->setText(tr("1 item"));
    } else {
        folderItemCountLabel_->setText(tr("%1 items").arg(itemCount));
    }

    if (folder.description.empty()) {
        folderDescriptionLabel_->setText(tr("No description"));
        folderDescriptionLabel_->setStyleSheet("font-size: 13px; color: #606060; font-style: italic;");
    } else {
        folderDescriptionLabel_->setText(QString::fromStdString(folder.description));
        folderDescriptionLabel_->setStyleSheet("font-size: 13px; color: #c0c0c0;");
    }

    stackedWidget_->setCurrentWidget(folderPage_);
}

void ConnectionDetailPanel::showEnvironment(const connections::domain::environment& env) {
    envNameLabel_->setText(QString::fromStdString(env.name));
    envHostLabel_->setText(QString::fromStdString(env.host));
    envPortLabel_->setText(QString::number(env.port));

    if (env.description.empty()) {
        envDescriptionLabel_->setText(tr("No description"));
        envDescriptionLabel_->setStyleSheet("font-size: 13px; color: #606060; font-style: italic;");
    } else {
        envDescriptionLabel_->setText(QString::fromStdString(env.description));
        envDescriptionLabel_->setStyleSheet("font-size: 13px; color: #c0c0c0;");
    }

    updateEnvironmentTagBadges(env.id);

    stackedWidget_->setCurrentWidget(environmentPage_);
}

void ConnectionDetailPanel::showConnection(const connections::domain::connection& conn) {
    connNameLabel_->setText(QString::fromStdString(conn.name));

    static const auto inheritedStyle =
        QString("font-size: 13px; color: #606060; font-style: italic;");
    static const auto normalStyle =
        QString("font-size: 13px; color: #c0c0c0;");

    // Resolve linked environment once if host/port are inherited
    std::optional<connections::domain::environment> resolvedEnv;
    QString envTooltip;
    if (conn.environment_id && (!conn.host || !conn.port)) {
        resolvedEnv = manager_->get_environment(*conn.environment_id);
        if (resolvedEnv) {
            envTooltip = tr("Inherited from environment: %1")
                .arg(QString::fromStdString(resolvedEnv->name));
        }
    }

    if (conn.host) {
        connHostLabel_->setText(QString::fromStdString(*conn.host));
        connHostLabel_->setStyleSheet(normalStyle);
        connHostLabel_->setToolTip({});
    } else if (resolvedEnv) {
        connHostLabel_->setText(QString::fromStdString(resolvedEnv->host));
        connHostLabel_->setStyleSheet(inheritedStyle);
        connHostLabel_->setToolTip(envTooltip);
    } else {
        connHostLabel_->setText({});
        connHostLabel_->setStyleSheet(normalStyle);
        connHostLabel_->setToolTip({});
    }

    if (conn.port) {
        connPortLabel_->setText(QString::number(*conn.port));
        connPortLabel_->setStyleSheet(normalStyle);
        connPortLabel_->setToolTip({});
    } else if (resolvedEnv) {
        connPortLabel_->setText(QString::number(resolvedEnv->port));
        connPortLabel_->setStyleSheet(inheritedStyle);
        connPortLabel_->setToolTip(envTooltip);
    } else {
        connPortLabel_->setText({});
        connPortLabel_->setStyleSheet(normalStyle);
        connPortLabel_->setToolTip({});
    }

    connUsernameLabel_->setText(QString::fromStdString(conn.username));

    if (conn.description.empty()) {
        connDescriptionLabel_->setText(tr("No description"));
        connDescriptionLabel_->setStyleSheet("font-size: 13px; color: #606060; font-style: italic;");
    } else {
        connDescriptionLabel_->setText(QString::fromStdString(conn.description));
        connDescriptionLabel_->setStyleSheet("font-size: 13px; color: #c0c0c0;");
    }

    updateConnectionTagBadges(conn.id, conn.environment_id);

    stackedWidget_->setCurrentWidget(connectionPage_);
}

void ConnectionDetailPanel::updateEnvironmentTagBadges(const boost::uuids::uuid& envId) {
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

        static_cast<QHBoxLayout*>(envTagsContainer_->layout())->addStretch();
        envTagsContainer_->setVisible(!tags.empty());
    } catch (const std::exception& e) {
        using namespace ores::logging;
        BOOST_LOG_SEV(lg(), error) << "Failed to load environment tags: " << e.what();
        envTagsContainer_->setVisible(false);
    }
}

void ConnectionDetailPanel::updateConnectionTagBadges(const boost::uuids::uuid& connId,
    const std::optional<boost::uuids::uuid>& envId) {

    QLayoutItem* item;
    while ((item = connTagsContainer_->layout()->takeAt(0)) != nullptr) {
        delete item->widget();
        delete item;
    }

    try {
        auto ownTags = manager_->get_tags_for_connection(connId);
        for (const auto& tag : ownTags) {
            auto* badge = createTagBadge(QString::fromStdString(tag.name), connTagsContainer_);
            connTagsContainer_->layout()->addWidget(badge);
        }

        // Show inherited environment tags as outlined badges
        bool hasAny = !ownTags.empty();
        if (envId) {
            auto envTags = manager_->get_tags_for_environment(*envId);
            for (const auto& tag : envTags) {
                QString name = QString::fromStdString(tag.name);
                QColor c = colorForTag(name);
                auto* badge = new QLabel(name, connTagsContainer_);
                badge->setStyleSheet(QString(
                    "QLabel {"
                    "  background-color: transparent;"
                    "  color: %1;"
                    "  border-radius: 8px;"
                    "  padding: 1px 7px;"
                    "  font-size: 11px;"
                    "  font-weight: bold;"
                    "  border: 1px solid %1;"
                    "}"
                ).arg(c.name()));
                badge->setToolTip(tr("Inherited from environment"));
                connTagsContainer_->layout()->addWidget(badge);
                hasAny = true;
            }
        }

        static_cast<QHBoxLayout*>(connTagsContainer_->layout())->addStretch();
        connTagsContainer_->setVisible(hasAny);
    } catch (const std::exception& e) {
        using namespace ores::logging;
        BOOST_LOG_SEV(lg(), error) << "Failed to load connection tags: " << e.what();
        connTagsContainer_->setVisible(false);
    }
}

}
