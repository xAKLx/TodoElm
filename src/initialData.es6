const today = new Date();
const tomorrow = new Date(new Date(today).setDate(today.getDate() + 1));

export const todoList = [
  {
    description: 'Send site plans and building floor plan brochure',
    lead: 'Chinmay Sarasvati',
    dateTime: new Date(today.setHours(11, 0, 0, 0)),
    tags: [
      { text: 'Created by Jason Shaw', color: '#44c89e' },
      { text: 'Site Visit', color: '#f9c492' },
      { text: 'Meeting', color: '#978efa' }
    ],
    closed: true
  },
  {
    description: 'Show floor manager site documents',
    lead: 'Olivia Evans',
    dateTime: new Date(today.setHours(13, 30, 0, 0)),
    tags: [
      { text: 'Inspection', color: '#fa90a8' }
    ],
    closed: false
  },
  {
    description: 'Meet client',
    lead: 'Erika Mateo',
    dateTime: new Date(tomorrow.setHours(11, 0, 0, 0)),
    tags: [
      { text: 'Demo', color: '#44c89e' },
      { text: 'Call', color: '#90eeec' },
      { text: 'Meeting', color: '#978efa' }
    ],
    closed: false
  },
  {
    description: 'Following up meeting',
    lead: 'Juan Jose Esteve',
    dateTime: new Date(tomorrow.setHours(12, 45, 0, 0)),
    tags: [
      { text: 'Call', color: '#90eeec' }
    ],
    closed: false
  },
  {
    description: 'Send site plan brochure',
    lead: 'Lucy Miller',
    dateTime: new Date(tomorrow.setHours(15, 0, 0, 0)),
    tags: [
      { text: 'Brochure', color: '#c6e99e' }
    ],
    closed: false
  },
  {
    description: 'Collect playment cheque from vendor. Deliver new merchendise to main office and take receipt.',
    lead: 'Seri Anand',
    dateTime: new Date(tomorrow.setHours(16, 15, 0, 0)),
    tags: [
      { text: 'Delivery', color: '#b0d3fb' }
    ],
    closed: false
  },
  {
    description: 'Meet client',
    lead: 'Erika Mateo',
    dateTime: new Date(tomorrow.setHours(11, 0, 0, 0)),
    tags: [
      { text: 'Demo', color: '#44c89e' },
      { text: 'Call', color: '#90eeec' },
      { text: 'Meeting', color: '#978efa' }
    ],
    closed: false
  },
  {
    description: 'Following up meeting',
    lead: 'Juan Jose Esteve',
    dateTime: new Date(tomorrow.setHours(12, 45, 0, 0)),
    tags: [
      { text: 'Call', color: '#90eeec' }
    ],
    closed: false
  },
  {
    description: 'Send site plan brochure',
    lead: 'Lucy Miller',
    dateTime: new Date(tomorrow.setHours(15, 0, 0, 0)),
    tags: [
      { text: 'Brochure', color: '#c6e99e' }
    ],
    closed: false
  },
  {
    description: 'Collect playment cheque from vendor. Deliver new merchendise to main office and take receipt.',
    lead: 'Seri Anand',
    dateTime: new Date(tomorrow.setHours(16, 15, 0, 0)),
    tags: [
      { text: 'Delivery', color: '#b0d3fb' }
    ],
    closed: false
  }
];