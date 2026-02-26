/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024-2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 */
#include "ores.qt/AboutDialog.hpp"

#include <QPixmap>
#include <QTreeWidget>
#include <QHeaderView>
#include "ores.utility/version/version.hpp"
#include "ores.comms/messaging/protocol.hpp"
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

using namespace ores::logging;

AboutDialog::AboutDialog(ClientManager* clientManager, QWidget* parent)
    : QWidget(parent), clientManager_(clientManager) {

    BOOST_LOG_SEV(lg(), debug) << "Creating about dialog.";
    ui_.setupUi(this);

    // Replace ui_.logoLabel with our custom LogoLabel in the layout
    logoLabel_ = new LogoLabel(ui_.logoContainer);
    logoLabel_->setAlignment(ui_.logoLabel->alignment());
    logoLabel_->setScaledContents(ui_.logoLabel->hasScaledContents());

    // Replace the widget in the layout
    QLayout* layout = ui_.logoLabel->parentWidget()->layout();
    if (layout) {
        layout->replaceWidget(ui_.logoLabel, logoLabel_);
        ui_.logoLabel->deleteLater();
    }

    // Component column auto-sizes; Version column takes the remaining space
    ui_.systemInfoTree->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    ui_.systemInfoTree->header()->setSectionResizeMode(1, QHeaderView::Stretch);
}

AboutDialog::~AboutDialog() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying about dialog.";
}

void AboutDialog::showEvent(QShowEvent* e) {
    QWidget::showEvent(e);

    const char* image(":/images/splash-screen.png");
    BOOST_LOG_SEV(lg(), debug) << "Scaling logo to fit the dialog. Image: "
                               << image;
    QPixmap logo(image);
    if (!logo.isNull()) {
        int targetWidth = width();
        BOOST_LOG_SEV(lg(), debug) << "Scaling to target width: " << targetWidth;

        QPixmap scaledLogo = logo.scaledToWidth(targetWidth, Qt::SmoothTransformation);

        logoLabel_->setPixmap(std::move(scaledLogo));

        const QString text = QString("v%1 %2 Protocol %3.%4")
                                 .arg(ORES_VERSION)
                                 .arg(ORES_BUILD_INFO)
                                 .arg(ores::comms::messaging::PROTOCOL_VERSION_MAJOR)
                                 .arg(ores::comms::messaging::PROTOCOL_VERSION_MINOR);
        logoLabel_->setTextOverlay(text);

        BOOST_LOG_SEV(lg(), debug) << "Scaled successfully.";
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Missing file: splash-screen.png";
    }

    populateSystemInfo();
}

void AboutDialog::populateSystemInfo() {
    ui_.systemInfoTree->clear();

    // Helper: extract value from KVP entries by key prefix
    const auto& entries = clientManager_
        ? clientManager_->systemInfoEntries()
        : std::vector<comms::messaging::system_info_entry>{};

    auto get = [&entries](const std::string& key) -> QString {
        for (const auto& e : entries) {
            if (e.key == key) {
                return QString::fromStdString(e.value);
            }
        }
        return {};
    };

    // Database row
    {
        auto* item = new QTreeWidgetItem(ui_.systemInfoTree);
        item->setText(0, "Database");

        const auto schema  = get("database.schema_version");
        const auto env     = get("database.build_environment");
        const auto commit  = get("database.git_commit");
        const auto date    = get("database.git_date");

        if (schema.isEmpty()) {
            item->setText(1, "(not connected)");
        } else {
            item->setText(1, QString("v%1 (%2 %3 %4)").arg(schema, env, commit, date));
        }
    }

    // Server row
    {
        auto* item = new QTreeWidgetItem(ui_.systemInfoTree);
        item->setText(0, "Server");

        const auto version  = get("server.version");
        const auto protocol = get("server.protocol_version");
        const auto build    = get("server.build_info");

        if (version.isEmpty()) {
            item->setText(1, "(not connected)");
        } else {
            item->setText(1, QString("v%1 Protocol %2 (%3)").arg(version, protocol, build));
        }
    }

    // Client row — always available (compile-time constants)
    {
        auto* item = new QTreeWidgetItem(ui_.systemInfoTree);
        item->setText(0, "Client");
        item->setText(1, QString("v%1 Protocol %2.%3 (%4)")
            .arg(ORES_VERSION)
            .arg(ores::comms::messaging::PROTOCOL_VERSION_MAJOR)
            .arg(ores::comms::messaging::PROTOCOL_VERSION_MINOR)
            .arg(ORES_BUILD_INFO));
    }

    // Shrink the Component column to its content; Version gets the rest
    ui_.systemInfoTree->resizeColumnToContents(0);
}

}
