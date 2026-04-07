/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_PARTYPICKERDIALOG_HPP
#define ORES_QT_PARTYPICKERDIALOG_HPP

#include <vector>
#include <QDialog>
#include <QLabel>
#include <QLineEdit>
#include <QComboBox>
#include <QRadioButton>
#include <QTimer>
#include <QTreeWidget>
#include <QPushButton>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/export.hpp"

namespace ores::qt {

class ImageCache;

/**
 * @brief Modal dialog for selecting a party from a list of available parties.
 *
 * Two mutually-exclusive sections, each headed by a native QRadioButton:
 *   - System section (hidden when the user has no system-party access).
 *   - Operational section with a filterable, alphabetically-sorted tree showing
 *     business-centre code (col 0, with flag icon) and party name (col 1).
 */
class ORES_QT_API PartyPickerDialog : public QDialog {
    Q_OBJECT

public:
    explicit PartyPickerDialog(
        const std::vector<PartyInfo>& parties,
        ClientManager* clientManager,
        ImageCache* imageCache,
        QWidget* parent = nullptr);

    boost::uuids::uuid selectedPartyId() const;
    QString selectedPartyName() const;

private slots:
    void onOkClicked();

private:
    enum Role {
        BusinessCentreRole = Qt::UserRole,
        PartyNameRole,
        PartyIdRole
    };

private:
    void setupUi();
    void populateCentreCombo();
    void applyFilter();
    void selectSystemParty();
    void selectOperationalItem(QTreeWidgetItem* item);
    QTreeWidgetItem* firstVisibleItem() const;
    void refreshFlagIcons();

private:
    ClientManager*         clientManager_;
    ImageCache*            imageCache_;
    std::vector<PartyInfo> parties_;

    // System-party section (hidden when no system party is available)
    QRadioButton* systemRadio_;

    // Operational-party section
    QRadioButton* operationalRadio_;
    QWidget*      operationalContent_;
    QLineEdit*    filterEdit_;
    QComboBox*    centreCombo_;
    QTreeWidget*  listWidget_;

    QPushButton* okButton_;
    QPushButton* cancelButton_;
    QTimer*      flagRefreshTimer_;

    // Tracks which party is currently selected
    boost::uuids::uuid selectedId_;
    QString            selectedName_;
};

}

#endif
